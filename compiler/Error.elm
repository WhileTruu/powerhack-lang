module Error exposing (Error(..), format)

import Console
import Data.FileContents as FileContents exposing (FileContents)
import Data.FilePath as FilePath exposing (FilePath)
import InferTypes
import Parse
import Parser.Advanced as P


type Error
    = ParseError Parse.Error
    | TypeError (List InferTypes.Error)


format : Error -> String
format error =
    case error of
        ParseError (Parse.Error deadEnds fileContents filePath) ->
            formatParseError filePath fileContents deadEnds

        TypeError typeErrors ->
            String.join "\n" (List.map InferTypes.errorToString typeErrors)


formatParseError : FilePath -> FileContents -> List (P.DeadEnd Parse.Context Parse.Problem) -> String
formatParseError filePath fileContents deadEnds =
    deadEndsToReports fileContents deadEnds
        |> List.map (renderReport filePath)
        |> String.join "\n"


deadEndsToReports : FileContents -> List (P.DeadEnd Parse.Context Parse.Problem) -> List Report
deadEndsToReports fileContents deadEnds =
    List.map (deadEndToReport fileContents) deadEnds


deadEndToReport : FileContents -> P.DeadEnd Parse.Context Parse.Problem -> Report
deadEndToReport fileContents deadEnd =
    { title =
        case deadEnd.contextStack of
            [] ->
                "PROBLEM WITH PARSING?"

            first :: _ ->
                "PROBLEM " ++ Parse.contextToString first.context
    , message =
        [ case deadEnd.contextStack of
            [] ->
                "I got stuck here:"

            first :: _ ->
                case first.context of
                    Parse.InLambda ->
                        "I got stuck parsing this lambda here:"

                    Parse.InDef ->
                        "I got stuck parsing this definition here:"

                    Parse.InDefs ->
                        "I got stuck parsing these definitions here:"

                    Parse.InIf ->
                        "I got stuck parsing this `if` expression here:"

                    Parse.InFile filePath ->
                        "I got stuck parsing this file here: " ++ FilePath.toString filePath
        , render fileContents deadEnd
        , case deadEnd.problem of
            Parse.ExpectingVarName ->
                "I was expecting to see a variable name."

            Parse.ExpectingDef ->
                "I was expecting to see a def."

            Parse.InvalidTab ->
                "FIXME: I don't like tabs."

            Parse.InvalidNumber ->
                "I was expecting to see a valid number."

            Parse.ExpectingNumber ->
                "I was expecting to see a number."

            Parse.ExpectingOpenParen ->
                "I was expecting to see an opening parenthesis next. Try putting a "
                    ++ Console.green "("
                    ++ " next and see if that helps?"

            Parse.ExpectingCloseParen ->
                "I was expecting to see a closing parenthesis next. Try putting a "
                    ++ Console.green ")"
                    ++ " next and see if that helps?"

            Parse.ExpectingEquals ->
                "I was expecting to see the equals sign (" ++ Console.green "=" ++ ") next."

            Parse.ExpectingBackslash ->
                "I was expecting to see a backslash next."

            Parse.ExpectingRightArrow ->
                "I was expecting to see the right arrow " ++ Console.green "->" ++ " next."

            Parse.FuncIdentBody ->
                "I was expecting the function body to be indented."

            Parse.ExpectingIndentation ->
                "I was expecting to see indentation next."

            Parse.ExpectingNoIndentation ->
                "I was expecting to see no indentation  next."

            Parse.ExpectingIf ->
                "I was expecting to see the " ++ Console.green "if" ++ " keyword next."

            Parse.ExpectingThen ->
                "I was expecting to see the " ++ Console.green "then" ++ " keyword next."

            Parse.ExpectingElse ->
                "I was expecting to see the " ++ Console.green "else" ++ " keyword next."

            Parse.ExpectingEnd ->
                "Whatever is here, I wasn't expecting it!"

            Parse.ExpectingImports ->
                "I was expecting to see the " ++ Console.green "imports" ++ " keyword next."

            Parse.ExpectingOpenBracket ->
                "I was expecting to see the open bracket (" ++ Console.green "[" ++ ") next."

            Parse.ExpectingComma ->
                "I was expecting to see the comma sign (" ++ Console.green "," ++ ") next."

            Parse.ExpectingCloseBracket ->
                "I was expecting to see the close bracket (" ++ Console.green "]" ++ ") next."

            Parse.ExpectingModuleName ->
                -- TODO Add better explanation
                "I was expecting to see the module name next."
                    ++ "Module names start with an uppercase letter and can only "
                    ++ "contain letters, numbers, and underscores."
        , deadEndToString deadEnd
        ]
            |> String.join "\n\n"
    }


render : FileContents -> P.DeadEnd Parse.Context Parse.Problem -> String
render fileContents deadEnd =
    let
        startLine : Int
        startLine =
            Maybe.withDefault deadEnd.row (Maybe.map .row (List.head deadEnd.contextStack))

        endLine : Int
        endLine =
            deadEnd.row

        relevantLines : List String
        relevantLines =
            FileContents.toString fileContents
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


renderReport : FilePath -> Report -> String
renderReport filePath report =
    let
        filePathStr : String
        filePathStr =
            FilePath.toString filePath
    in
    [ Console.cyan
        ("-- "
            ++ String.padRight (80 - 4 - String.length filePathStr - 1)
                '-'
                (report.title ++ " ")
            ++ " "
            ++ filePathStr
        )
    , ""
    , report.message
    , ""
    , ""
    ]
        |> String.join "\n"


deadEndToString : P.DeadEnd Parse.Context Parse.Problem -> String
deadEndToString { row, col, problem, contextStack } =
    String.fromInt row
        ++ ":"
        ++ String.fromInt col
        ++ " "
        ++ Parse.problemToString problem
        ++ " <--\n    "
        ++ String.join " <-- " (List.map contextToString contextStack)


contextToString : { row : Int, col : Int, context : Parse.Context } -> String
contextToString { row, col, context } =
    String.fromInt row ++ ":" ++ String.fromInt col ++ " " ++ Parse.contextToString context
