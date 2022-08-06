module Compiler exposing (..)

import AssocList as Dict
import Canonicalize
import Data.FileContents as FileContents exposing (FileContents)
import Data.FilePath as FilePath exposing (FilePath)
import Data.Name as Name
import Error exposing (Error)
import Expect
import Fuzz
import Generate
import InferTypes
import Parse
import Parse.Expression
import Parser.Advanced as P
import Random exposing (Generator)
import Random.Extra as Random
import Random.String
import Shrink
import String.Extra
import Test exposing (Test)


compile : FilePath -> FileContents -> Result Error String
compile filePath fileContents =
    Parse.parse filePath fileContents
        |> Result.andThen Canonicalize.canonicalize
        |> Result.andThen
            (\a ->
                case InferTypes.run a of
                    Ok _ ->
                        Ok a

                    Err errors ->
                        Err (Error.TypeError errors)
            )
        |> Result.map Generate.generate
        >> Result.map ((++) (builtIns ++ "\n\n"))
        >> Result.map (\a -> a ++ "\n\nconsole.log((function () { return main() })())")


builtIns : String
builtIns =
    [ "#!/usr/bin/env node"
    , ""

    -- FIXME These need to match the primitives in InferTypes module
    , "var add = function (b) { return function (a) { return a + b; }; }"
    , "var sub = function (b) { return function (a) { return a - b; }; }"
    , "var eq = function (b) { return function (a) { return a === b; }; }"
    ]
        |> String.join "\n"


parseAndCompileTestSuite : Test
parseAndCompileTestSuite =
    let
        parseAndGenerate : String -> Result Error String
        parseAndGenerate input =
            Parse.parse (FilePath.init "Test.powerhack") (FileContents.init input)
                |> Result.andThen Canonicalize.canonicalize
                |> Result.map Generate.generate
    in
    Test.describe "Parse and compile"
        [ Test.fuzz2 varNameFuzzer (Fuzz.map abs Fuzz.int) "variable" <|
            \varName int ->
                let
                    input : String
                    input =
                        varName ++ " = " ++ String.fromInt int

                    output : String
                    output =
                        "var " ++ varName ++ " = " ++ String.fromInt int
                in
                Expect.equal (Ok output) (parseAndGenerate input)
        , Test.fuzz varNameFuzzer "function" <|
            \varName ->
                let
                    input : String
                    input =
                        varName ++ " = \\a b c d -> 3"

                    output : String
                    output =
                        [ "var " ++ varName ++ " = function (a) {"
                        , "    return function (b) {"
                        , "        return function (c) {"
                        , "            return function (d) {"
                        , "                return 3"
                        , "            }"
                        , "        }"
                        , "    }"
                        , "}"
                        ]
                            |> String.join "\n"
                in
                Expect.equal (Ok output) (parseAndGenerate input)
        , Test.fuzz varNameFuzzer "nested functions" <|
            \varName ->
                let
                    input : String
                    input =
                        varName ++ " = \\a b -> \\c d -> 3"

                    output : String
                    output =
                        [ "var " ++ varName ++ " = function (a) {"
                        , "    return function (b) {"
                        , "        return function (c) {"
                        , "            return function (d) {"
                        , "                return 3"
                        , "            }"
                        , "        }"
                        , "    }"
                        , "}"
                        ]
                            |> String.join "\n"
                in
                Expect.equal (Ok output) (parseAndGenerate input)
        , Test.fuzz varNameFuzzer "nested function formatted on multiple lines" <|
            \varName ->
                let
                    input : String
                    input =
                        [ varName ++ " ="
                        , "    \\a b ->"
                        , "        \\c d ->"
                        , "            3"
                        ]
                            |> String.join "\n"

                    output : String
                    output =
                        [ "var " ++ varName ++ " = function (a) {"
                        , "    return function (b) {"
                        , "        return function (c) {"
                        , "            return function (d) {"
                        , "                return 3"
                        , "            }"
                        , "        }"
                        , "    }"
                        , "}"
                        ]
                            |> String.join "\n"
                in
                Expect.equal (Ok output) (parseAndGenerate input)
        , Test.test "definition in function" <|
            \_ ->
                let
                    input : String
                    input =
                        [ "varName ="
                        , "    \\a ->"
                        , "        x = 1"
                        , ""
                        , "        bar x"
                        ]
                            |> String.join "\n"

                    output : String
                    output =
                        [ "var varName = function (a) {"
                        , "    return (function () {"
                        , "        var x = 1"
                        , "        return bar(x)"
                        , "    })()"
                        , "}"
                        ]
                            |> String.join "\n"
                in
                Expect.equal (Ok output) (parseAndGenerate input)
        , Test.test "multiple definitions in function" <|
            \_ ->
                let
                    input : String
                    input =
                        [ "varName ="
                        , "    \\a ->"
                        , "        x = 1"
                        , "        y = 2"
                        , ""
                        , "        bar x"
                        ]
                            |> String.join "\n"

                    output : String
                    output =
                        [ "var varName = function (a) {"
                        , "    return (function () {"
                        , "        var x = 1"
                        , "        var y = 2"
                        , "        return bar(x)"
                        , "    })()"
                        , "}"
                        ]
                            |> String.join "\n"
                in
                Expect.equal (Ok output) (parseAndGenerate input)
        , Test.test "multiple definitions in function that returns function" <|
            \_ ->
                let
                    input : String
                    input =
                        [ "varName ="
                        , "    \\a ->"
                        , "        x = 1"
                        , "        y = 2"
                        , ""
                        , "        \\b -> bar x"
                        ]
                            |> String.join "\n"

                    output : String
                    output =
                        [ "var varName = function (a) {"
                        , "    return (function () {"
                        , "        var x = 1"
                        , "        var y = 2"
                        , "        return function (b) {"
                        , "            return bar(x)"
                        , "        }"
                        , "    })()"
                        , "}"
                        ]
                            |> String.join "\n"
                in
                Expect.equal (Ok output) (parseAndGenerate input)
        , Test.test "if then else" <|
            \_ ->
                let
                    input : String
                    input =
                        "varName = if a then 1 else 2"

                    output : String
                    output =
                        [ "var varName = (function () {"
                        , "    if (a) {"
                        , "        return 1"
                        , "    } else {"
                        , "        return 2"
                        , "    }"
                        , "})()"
                        ]
                            |> String.join "\n"
                in
                Expect.equal (Ok output) (parseAndGenerate input)
        ]


inferTypesTestSuite : Test
inferTypesTestSuite =
    let
        parseExprAndInferTypes : String -> Result String String
        parseExprAndInferTypes input =
            P.run Parse.Expression.expression input
                |> Result.mapError Debug.toString
                |> Result.andThen (Result.mapError Debug.toString << Canonicalize.canonicalizeExpr)
                |> Result.andThen (Result.mapError Debug.toString << InferTypes.runForExpr)
                |> Result.map InferTypes.prettyScheme

        parseAndInferType : String -> Result String String
        parseAndInferType input =
            Parse.parse (FilePath.init "Test.powerhack") (FileContents.init input)
                |> Result.mapError Debug.toString
                |> Result.andThen (Result.mapError Debug.toString << Canonicalize.canonicalize)
                |> Result.andThen (Result.mapError Debug.toString << InferTypes.run)
                |> Result.map
                    (Dict.foldl
                        (\k v a ->
                            a
                                ++ Name.toString k
                                ++ ": "
                                ++ InferTypes.prettyScheme v
                                ++ "\n"
                        )
                        ""
                    )
                |> Result.map (String.dropRight 1)

        primitiveTypes : List String
        primitiveTypes =
            [ "eq: Int -> Int -> Bool"
            , "sub: Int -> Int -> Int"
            , "add: Int -> Int -> Int"
            ]
    in
    Test.describe "Infer types"
        [ Test.test "variable" <|
            \_ ->
                "\\a -> a"
                    |> parseExprAndInferTypes
                    |> Expect.equal (Ok "∀ a. a -> a")
        , Test.test "variable 2" <|
            \_ ->
                """
                \\a ->
                    x = 1
                    1
                """
                    |> (String.Extra.unindent >> String.trim)
                    |> parseExprAndInferTypes
                    |> Expect.equal (Ok "∀ a. a -> Int")
        , Test.test "variable 3" <|
            \_ ->
                """
                \\x ->
                    fib = \\n a b ->
                        if eq 0 n then
                            a
                        else
                            fib (sub 1 n) b (add b a)
                    
                    fib 10 0 1
                """
                    |> (String.Extra.unindent >> String.trim)
                    |> parseExprAndInferTypes
                    |> Expect.equal (Ok "∀ a. a -> Int")
        , Test.test "parse & infer type" <|
            \_ ->
                let
                    expected : String
                    expected =
                        [ "fib: Int -> Int -> Int -> Int"
                        , "main: ∀ a. a -> Int"
                        ]
                            |> (++) primitiveTypes
                            |> String.join "\n"
                in
                """
                main = \\arggg ->
                    fib 10 0 1

                fib = \\n a b ->
                    if eq 0 n then
                        a
                    else
                        fib (sub 1 n) b (add b a)
                """
                    |> (String.Extra.unindent >> String.trim)
                    |> parseAndInferType
                    |> Expect.equal (Ok expected)
        , Test.test "parse & infer type basic" <|
            \_ ->
                let
                    input : String
                    input =
                        "foo = \\a -> 1"

                    expected : String
                    expected =
                        [ "foo: ∀ a. a -> Int"
                        ]
                            |> (++) primitiveTypes
                            |> String.join "\n"
                in
                parseAndInferType input
                    |> Expect.equal (Ok expected)
        , Test.test "unbound variable" <|
            \_ ->
                let
                    input : String
                    input =
                        [ "foo = \\a -> potato 1"
                        , ""
                        ]
                            |> String.join "\n"

                    expected : String
                    expected =
                        "[UnboundVariable (Name \"potato\"),UnboundVariable (Name \"potato\")]"
                in
                parseAndInferType input
                    |> Expect.equal (Err expected)
        , Test.skip <|
            Test.test "recursive value" <|
                -- FIXME some sort of cycle detection thing?
                -- https://gist.github.com/evancz/07436448b7d6c947f21742dab46d1218
                \_ ->
                    "x = add x 1"
                        |> parseAndInferType
                        |> Expect.err
        , Test.test "unification fail" <|
            \_ ->
                let
                    input : String
                    input =
                        [ "bar = \\x -> foo 1 1"
                        , "foo = \\a -> add a 1"
                        ]
                            |> String.join "\n"

                    expected : String
                    expected =
                        "[UnificationFail (TypeApplied (Name \"Int\") []) (TypeLambda (TypeApplied (Name \"Int\") []) (TypeVar (Name \"u20\"))),UnificationFail (TypeApplied (Name \"Int\") []) (TypeLambda (TypeApplied (Name \"Int\") []) (TypeVar (Name \"u29\")))]"
                in
                parseAndInferType input
                    |> Expect.equal (Err expected)
        , let
            fuzzer : Fuzz.Fuzzer String
            fuzzer =
                Fuzz.map2
                    (\a b ->
                        List.sortBy Tuple.first [ a, b ]
                            |> List.map Tuple.second
                            |> String.join "\n"
                    )
                    (Fuzz.map (\a -> ( a, "foo = \\a -> add a 1" )) Fuzz.int)
                    (Fuzz.map (\a -> ( a, "bar = \\x -> foo 1" )) Fuzz.int)
          in
          Test.fuzz fuzzer "definition order does not affect type" <|
            \input ->
                let
                    expected : List String
                    expected =
                        "foo: Int -> Int" :: "bar: ∀ a. a -> Int" :: primitiveTypes
                in
                parseAndInferType input
                    |> Expect.all
                        (List.map
                            (\a ->
                                Expect.true ("Expected types to contain " ++ a)
                                    << Result.withDefault False
                                    << Result.map (String.contains a)
                            )
                            expected
                        )
        , Test.test "infinite type from bind" <|
            \_ ->
                let
                    expected : String
                    expected =
                        "[InfiniteTypeFromBind (TypeVar (Name \"u2\")) (TypeLambda (TypeVar (Name \"u4\")) (TypeVar (Name \"u2\")))]"
                in
                "foo = \\a -> foo"
                    |> parseAndInferType
                    |> Expect.equal (Err expected)
        , Test.todo "infinite type from occurs"
        ]


varNameFuzzer : Fuzz.Fuzzer String
varNameFuzzer =
    let
        digitCharGenerator : Generator Char
        digitCharGenerator =
            Random.map Char.fromCode (Random.int 48 57)

        upperCaseAlphaCharGenerator : Generator Char
        upperCaseAlphaCharGenerator =
            Random.map Char.fromCode (Random.int 65 90)

        lowerCaseAlphaCharGenerator : Generator Char
        lowerCaseAlphaCharGenerator =
            Random.map Char.fromCode (Random.int 97 122)

        validCharGenerator : Generator Char
        validCharGenerator =
            Random.choices digitCharGenerator [ upperCaseAlphaCharGenerator, lowerCaseAlphaCharGenerator ]

        asciiGenerator : Random.Generator String
        asciiGenerator =
            Random.frequency
                ( 3, Random.int 1 10 )
                [ ( 0.2, Random.constant 0 )
                , ( 1, Random.int 11 50 )
                , ( 1, Random.int 50 1000 )
                ]
                |> Random.andThen (\len -> Random.String.string len validCharGenerator)
                |> Random.andThen (\str -> Random.map (\a -> String.cons a str) lowerCaseAlphaCharGenerator)
    in
    Fuzz.map2 String.cons
        (Fuzz.custom lowerCaseAlphaCharGenerator (Shrink.atLeastChar 'a'))
        (Fuzz.custom asciiGenerator Shrink.string)
