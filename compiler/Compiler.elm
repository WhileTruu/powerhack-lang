module Compiler exposing (..)

import Expect
import Fuzz
import Generate
import Parse
import Parse.Error
import Parser.Advanced as PA
import Random exposing (Generator)
import Random.Extra as Random
import Random.String
import Shrink
import Test exposing (Test)


compile : String -> Result (List (PA.DeadEnd Parse.Error.Context Parse.Error.Problem)) String
compile =
    compileDecls
        >> Result.map ((++) (builtIns ++ "\n\n"))
        >> Result.map (\a -> a ++ "\n\nconsole.log((function () { return main() })())")


compileDecls : String -> Result (List (PA.DeadEnd Parse.Error.Context Parse.Error.Problem)) String
compileDecls =
    PA.run Parse.decls
        >> Result.map (List.map Generate.generate)
        >> Result.map (String.join "\n\n")


builtIns : String
builtIns =
    [ "#!/usr/bin/env node"
    , ""
    , "function add(b, a) { return a + b; }"
    , "function sub(b, a) { return a - b; }"
    , "function eq(b, a) { return a === b; }"
    ]
        |> String.join "\n"


parseAndCompileSuite : Test
parseAndCompileSuite =
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
                Expect.equal (Ok output) (compileDecls input)
        , Test.fuzz varNameFuzzer "function" <|
            \varName ->
                let
                    input : String
                    input =
                        varName ++ " = \\a b c d -> 3"

                    output : String
                    output =
                        [ "var " ++ varName ++ " = function (a, b, c, d) {"
                        , "    return 3"
                        , "}"
                        ]
                            |> String.join "\n"
                in
                Expect.equal (Ok output) (compileDecls input)
        , Test.fuzz varNameFuzzer "nested functions" <|
            \varName ->
                let
                    input : String
                    input =
                        varName ++ " = \\a b -> \\c d -> 3"

                    output : String
                    output =
                        [ "var " ++ varName ++ " = function (a, b) {"
                        , "    return function (c, d) {"
                        , "        return 3"
                        , "    }"
                        , "}"
                        ]
                            |> String.join "\n"
                in
                Expect.equal (Ok output) (compileDecls input)
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
                        [ "var " ++ varName ++ " = function (a, b) {"
                        , "    return function (c, d) {"
                        , "        return 3"
                        , "    }"
                        , "}"
                        ]
                            |> String.join "\n"
                in
                Expect.equal (Ok output) (compileDecls input)
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
                Expect.equal (Ok output) (compileDecls input)
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
                Expect.equal (Ok output) (compileDecls input)
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
                Expect.equal (Ok output) (compileDecls input)
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
                Expect.equal (Ok output) (compileDecls input)
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
