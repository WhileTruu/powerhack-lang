module Error exposing (CanonicalizationError, Error(..), format)

import Console
import Data.FileContents as FileContents exposing (FileContents)
import Data.FilePath as FilePath exposing (FilePath)
import Parse.Error as E
import Parser.Advanced as P


type Error
    = ParseError (List (P.DeadEnd E.Context E.Problem)) FileContents FilePath
    | CanonicalizationError CanonicalizationError


format : Error -> String
format error =
    case error of
        ParseError deadEnds fileContents filePath ->
            formatParseError filePath fileContents deadEnds

        CanonicalizationError _ ->
            "FIXME: add like errors here"


type alias CanonicalizationError =
    ()


formatParseError : FilePath -> FileContents -> List (P.DeadEnd E.Context E.Problem) -> String
formatParseError filePath fileContents deadEnds =
    deadEndsToReports fileContents deadEnds
        |> List.map (renderReport filePath)
        |> String.join "\n"


deadEndsToReports : FileContents -> List (P.DeadEnd E.Context E.Problem) -> List Report
deadEndsToReports fileContents deadEnds =
    List.map (deadEndToReport fileContents) deadEnds


deadEndToReport : FileContents -> P.DeadEnd E.Context E.Problem -> Report
deadEndToReport fileContents deadEnd =
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

                    E.InFile filePath ->
                        "I got stuck parsing this file here: " ++ FilePath.toString filePath
        , render fileContents deadEnd
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


render : FileContents -> P.DeadEnd E.Context E.Problem -> String
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
