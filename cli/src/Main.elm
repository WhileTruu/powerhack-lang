module Main exposing (..)

import Compiler
import Data.FileContents as FileContents
import Data.FilePath as FilePath
import Error
import List.Extra as List
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as IOFile
import Posix.IO.File.Permission as IOFilePermission


program : Process -> IO x ()
program process =
    let
        processArgs : ProcessArgs
        processArgs =
            processArgsFromStrings (List.drop 1 process.argv)
    in
    Maybe.map2 (\file output -> readCompileAndWrite { file = file, output = output })
        processArgs.file
        processArgs.output
        |> Maybe.withDefault (IO.printLn "Error: invalid args")


readCompileAndWrite : { file : String, output : String } -> IO x ()
readCompileAndWrite { file, output } =
    IOFile.read file
        |> IO.andThen
            (\s ->
                case Compiler.compile (FilePath.init file) (FileContents.init s) of
                    Ok outputS ->
                        [ "Success! Compiled 1 module."
                        , ""
                        , "    " ++ file ++ " ───> " ++ output
                        , ""
                        , ""
                        ]
                            |> String.join "\n"
                            |> IO.print
                            |> IO.map (\_ -> outputS)

                    Err error ->
                        Error.format error
                            |> IO.fail
            )
        |> IO.andThen
            (IOFile.write
                (IOFile.CreateIfNotExists IOFile.Truncate IOFilePermission.default)
                output
            )
        |> IO.recover IO.print



-- PROCESS ARGS


type alias ProcessArgs =
    { file : Maybe String
    , output : Maybe String
    , unknowns : List String
    }


processArgsFromStrings : List String -> ProcessArgs
processArgsFromStrings args =
    let
        ( args2, output ) =
            getOutputArg [] args
    in
    { file = List.find (String.endsWith ".powerhack") args2
    , output = output
    , unknowns = removeIf (String.endsWith ".powerhack") args2
    }


getOutputArg : List String -> List String -> ( List String, Maybe String )
getOutputArg prev args =
    case args of
        first :: second :: others ->
            if first == "--output" then
                ( List.reverse prev ++ others, Just second )

            else
                getOutputArg (first :: prev) (second :: others)

        _ ->
            ( List.reverse prev ++ args, Nothing )


removeIf : (a -> Bool) -> List a -> List a
removeIf predicate list =
    removeIfHelp predicate list []


removeIfHelp : (a -> Bool) -> List a -> List a -> List a
removeIfHelp predicate remaining accumulator =
    case remaining of
        [] ->
            List.reverse accumulator ++ remaining

        first :: rest ->
            if predicate first then
                List.reverse accumulator ++ rest

            else
                removeIfHelp predicate rest (first :: accumulator)
