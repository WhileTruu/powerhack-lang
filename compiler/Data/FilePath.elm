module Data.FilePath exposing (FilePath, init, toString)


type FilePath
    = FilePath String


init : String -> FilePath
init fileContents =
    FilePath fileContents


toString : FilePath -> String
toString (FilePath fileContents) =
    fileContents
