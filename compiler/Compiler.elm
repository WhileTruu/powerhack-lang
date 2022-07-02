module Compiler exposing (..)

import Console
import Expect
import Fuzz
import Generate
import Parse
import Parse.Error as E
import Parser.Advanced as P
import Random exposing (Generator)
import Random.Extra as Random
import Random.String
import Shrink
import Test exposing (Test)


compile : String -> Result (List (P.DeadEnd E.Context E.Problem)) String
compile =
    compileDecls
        >> Result.map ((++) (builtIns ++ "\n\n"))
        >> Result.map (\a -> a ++ "\n\nconsole.log((function () { return main() })())")


compileDecls : String -> Result (List (P.DeadEnd E.Context E.Problem)) String
compileDecls =
    P.run Parse.decls
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


formatParseError : { fileName : String } -> String -> List (P.DeadEnd E.Context E.Problem) -> String
formatParseError { fileName } source deadEnds =
    deadEndsToReports source deadEnds
        |> List.map (renderReport { fileName = fileName })
        |> String.join "\n"


deadEndsToReports : String -> List (P.DeadEnd E.Context E.Problem) -> List Report
deadEndsToReports source deadEnds =
    List.map (deadEndToReport source) deadEnds


deadEndToReport : String -> P.DeadEnd E.Context E.Problem -> Report
deadEndToReport source deadEnd =
    { title =
        case deadEnd.contextStack of
            [] ->
                "PROBLEM WITH PARSING?"

            first :: _ ->
                "PROBLEM " ++ E.contextToString first.context
    , message =
        [ case deadEnd.contextStack of
            [] ->
                "I got stuck here:"

            first :: _ ->
                case first.context of
                    E.InDeclaration ->
                        "I got stuck parsing this declaration here:"

                    E.InLambda ->
                        "I got stuck parsing this lambda here:"

                    E.InDef ->
                        "I got stuck parsing this definition here:"

                    E.InDefs ->
                        "I got stuck parsing these definitions here:"

                    E.InIf ->
                        "I got stuck parsing this `if` expression here:"
        , render source deadEnd
        , case deadEnd.problem of
            E.ExpectingVarName ->
                "I was expecting to see a variable name."

            E.ExpectingDef ->
                "I was expecting to see a def."

            E.InvalidTab ->
                "FIXME: I don't like tabs."

            E.InvalidNumber ->
                "I was expecting to see a valid number."

            E.ExpectingNumber ->
                "I was expecting to see a number."

            E.ExpectingOpenParen ->
                "I was expecting to see an opening parenthesis next. Try putting a "
                    ++ Console.green "("
                    ++ " next and see if that helps?"

            E.ExpectingCloseParen ->
                "I was expecting to see a closing parenthesis next. Try putting a "
                    ++ Console.green ")"
                    ++ " next and see if that helps?"

            E.ExpectingComma ->
                "I was expecting to see a comma (" ++ Console.green "," ++ ") next."

            E.ExpectingEquals ->
                "I was expecting to see the equals sign (" ++ Console.green "=" ++ ") next."

            E.ExpectingBackslash ->
                "I was expecting to see a backslash next."

            E.ExpectingRightArrow ->
                "I was expecting to see the right arrow " ++ Console.green "->" ++ " next."

            E.FuncIdentBody ->
                "I was expecting the function body to be indented."

            E.ExpectingIndentation ->
                "I was expecting to see indentation next."

            E.ExpectingNoIndentation ->
                "I was expecting to see no indentation  next."

            E.ExpectingIf ->
                "I was expecting to see the " ++ Console.green "if" ++ " keyword next."

            E.ExpectingThen ->
                "I was expecting to see the " ++ Console.green "then" ++ " keyword next."

            E.ExpectingElse ->
                "I was expecting to see the " ++ Console.green "else" ++ " keyword next."
        , deadEndToString deadEnd
        ]
            |> String.join "\n\n"
    }


render : String -> P.DeadEnd E.Context E.Problem -> String
render source deadEnd =
    let
        startLine : Int
        startLine =
            Maybe.withDefault deadEnd.row (Maybe.map .row (List.head deadEnd.contextStack))

        endLine : Int
        endLine =
            deadEnd.row

        relevantLines : List String
        relevantLines =
            source
                |> String.split "\n"
                |> List.drop (startLine - 1)
                |> List.take (1 + endLine - startLine)

        largestLineNumberDecimalCount : Int
        largestLineNumberDecimalCount =
            String.length (String.fromInt endLine)
    in
    relevantLines
        |> addLineNumbers startLine
        |> (\a ->
                a
                    ++ [ String.repeat (largestLineNumberDecimalCount + 1 + deadEnd.col) " "
                            ++ Console.red "^"
                       ]
           )
        |> String.join "\n"


addLineNumbers : Int -> List String -> List String
addLineNumbers start lines =
    let
        end : Int
        end =
            start + List.length lines

        largestLineNumberDecimalCount : Int
        largestLineNumberDecimalCount =
            String.length (String.fromInt end)
    in
    List.indexedMap
        (\i line ->
            String.padLeft (largestLineNumberDecimalCount + 1)
                ' '
                (String.fromInt (start + i) ++ "| ")
                ++ line
        )
        lines


type alias Report =
    { title : String
    , message : String
    }


renderReport : { fileName : String } -> Report -> String
renderReport { fileName } report =
    [ Console.cyan ("-- " ++ String.padRight (80 - 4 - String.length fileName - 1) '-' (report.title ++ " ") ++ " " ++ fileName)
    , ""
    , report.message
    , ""
    , ""
    ]
        |> String.join "\n"


deadEndToString : P.DeadEnd E.Context E.Problem -> String
deadEndToString { row, col, problem, contextStack } =
    String.fromInt row
        ++ ":"
        ++ String.fromInt col
        ++ " "
        ++ E.problemToString problem
        ++ " <--\n    "
        ++ String.join " <-- " (List.map contextToString contextStack)


contextToString : { row : Int, col : Int, context : E.Context } -> String
contextToString { row, col, context } =
    String.fromInt row ++ ":" ++ String.fromInt col ++ " " ++ E.contextToString context
