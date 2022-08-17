module Report exposing (Report, renderErrorInFileContents, renderReport)

import Console
import Data.FileContents as FileContents exposing (FileContents)
import Data.FilePath as FilePath exposing (FilePath)


renderErrorInFileContents : FileContents -> { startRow : Int, row : Int, col : Int } -> String
renderErrorInFileContents fileContents { startRow, row, col } =
    let
        relevantLines : List String
        relevantLines =
            FileContents.toString fileContents
                |> String.split "\n"
                |> List.drop (startRow - 1)
                |> List.take (1 + row - startRow)

        largestLineNumberDecimalCount : Int
        largestLineNumberDecimalCount =
            String.length (String.fromInt row)
    in
    relevantLines
        |> addLineNumbers startRow
        |> (\a ->
                a
                    ++ [ String.repeat (largestLineNumberDecimalCount + 1 + col) " "
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
    { filePath : FilePath
    , title : String
    , message : String
    }


renderReport : Report -> String
renderReport report =
    let
        filePathStr : String
        filePathStr =
            FilePath.toString report.filePath
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
