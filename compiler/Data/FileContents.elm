module Data.FileContents exposing (FileContents, init, toString)


type FileContents
    = FileContents String


init : String -> FileContents
init fileContents =
    FileContents fileContents


toString : FileContents -> String
toString (FileContents fileContents) =
    fileContents
