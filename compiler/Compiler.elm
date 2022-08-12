module Compiler exposing (compile, inferTypesTestSuite)

import AssocList as Dict
import Canonicalize
import Data.FileContents as FileContents exposing (FileContents)
import Data.FilePath as FilePath exposing (FilePath)
import Data.Name as Name
import Emit
import Error exposing (Error)
import Expect
import Fuzz
import InferTypes
import Parse
import Parse.Expression
import Parser.Advanced as P
import String.Extra
import Test exposing (Test)


compile : FilePath -> FileContents -> Emit.Format -> Result Error String
compile filePath fileContents format =
    Parse.run filePath fileContents
        |> Result.map Canonicalize.canonicalize
        |> Result.andThen
            (\a ->
                case InferTypes.run a of
                    Ok ( module_, _ ) ->
                        Ok module_

                    Err errors ->
                        Err (Error.TypeError errors)
            )
        |> Result.map (Emit.run format)


inferTypesTestSuite : Test
inferTypesTestSuite =
    let
        parseExprAndInferTypes : String -> Result Error String
        parseExprAndInferTypes input =
            P.run Parse.Expression.expression input
                |> Result.mapError (\a -> Error.ParseError a (FileContents.init input) (FilePath.init "Fake.powerhack"))
                |> Result.map Canonicalize.canonicalizeExpr
                |> Result.andThen (Result.mapError Error.TypeError << InferTypes.runForExpr)
                |> Result.map Tuple.first
                |> Result.map InferTypes.prettyScheme

        parseAndInferType : String -> Result Error String
        parseAndInferType input =
            Parse.run (FilePath.init "Test.powerhack") (FileContents.init input)
                |> Result.map Canonicalize.canonicalize
                |> Result.andThen (Result.mapError Error.TypeError << InferTypes.run)
                |> Result.map Tuple.second
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
                        [ "main: ∀ a. a -> Int"
                        , "fib: Int -> Int -> Int -> Int"
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
        , Test.skip <|
            Test.test "recursive value" <|
                -- FIXME some sort of cycle detection thing?
                -- https://gist.github.com/evancz/07436448b7d6c947f21742dab46d1218
                \_ ->
                    "x = add x 1"
                        |> parseAndInferType
                        |> Expect.err
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
        , Test.todo "infinite type from occurs"
        ]
