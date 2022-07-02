module Parse exposing (parse)

import AST.Source as Source
import Data.FileContents as FileContents exposing (FileContents)
import Data.FilePath exposing (FilePath)
import Error exposing (Error)
import Parse.Module
import Parser.Advanced as P


parse : FilePath -> FileContents -> Result Error Source.Module
parse filePath fileContents =
    P.run (Parse.Module.module_ filePath) (FileContents.toString fileContents)
        |> Result.mapError (\a -> Error.ParseError a fileContents filePath)
