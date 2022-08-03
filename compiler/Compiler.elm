module Compiler exposing (..)

import Canonicalize
import Data.FileContents as FileContents exposing (FileContents)
import Data.FilePath as FilePath exposing (FilePath)
import Error exposing (Error)
import Expect
import Fuzz
import Generate
import Parse
import Random exposing (Generator)
import Random.Extra as Random
import Random.String
import Shrink
import Test exposing (Test)


compile : FilePath -> FileContents -> Result Error String
compile filePath fileContents =
    Parse.parse filePath fileContents
        |> Result.andThen Canonicalize.canonicalize
        |> Result.map Generate.generate
        >> Result.map ((++) (builtIns ++ "\n\n"))
        >> Result.map (\a -> a ++ "\n\nconsole.log((function () { return main() })())")


builtIns : String
builtIns =
    [ "#!/usr/bin/env node"
    , ""
    , "var add = function (b) { return function (a) { return a + b; }; }"
    , "var sub = function (b) { return function (a) { return a - b; }; }"
    , "var eq = function (b) { return function (a) { return a === b; }; }"
    ]
        |> String.join "\n"


parseAndCompileSuite : Test
parseAndCompileSuite =
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
