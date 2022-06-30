module Main exposing (..)

import Compiler
import List.Extra as List
import Parse.Error
import Parser.Advanced as PA exposing ((|.))
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
    Maybe.map2 (\file output -> appleSauce { file = file, output = output })
        processArgs.file
        processArgs.output
        |> Maybe.withDefault (IO.printLn "Error: invalid args")


appleSauce : { file : String, output : String } -> IO x ()
appleSauce { file, output } =
    IOFile.read file
        |> IO.andThen
            (\s ->
                case Compiler.compile s of
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

                    Err deadEnds ->
                        [ "Detected problems in 1 module."
                        , header { title = "SOME ERROR", location = file }
                        , ""
                        , addLineNumbers s
                        , ""
                        , deadEndsToString deadEnds
                        , ""
                        , ""
                        ]
                            |> String.join "\n"
                            |> IO.fail
            )
        |> IO.andThen
            (IOFile.write
                (IOFile.CreateIfNotExists IOFile.Truncate IOFilePermission.default)
                output
            )
        |> IO.recover IO.print


header : { title : String, location : String } -> String
header { title, location } =
    "-- " ++ String.padRight (80 - 4 - String.length location - 1) '-' (title ++ " ") ++ " " ++ location


deadEndsToString : List (PA.DeadEnd Parse.Error.Context Parse.Error.Problem) -> String
deadEndsToString deadEnds =
    String.join "\n" (List.map deadEndToString deadEnds)


deadEndToString : PA.DeadEnd Parse.Error.Context Parse.Error.Problem -> String
deadEndToString { row, col, problem, contextStack } =
    String.fromInt row
        ++ ":"
        ++ String.fromInt col
        ++ " "
        ++ Parse.Error.problemToString problem
        ++ " - "
        ++ String.join " " (List.map contextToString contextStack)


contextToString : { row : Int, col : Int, context : Parse.Error.Context } -> String
contextToString { row, col, context } =
    String.fromInt row ++ ":" ++ String.fromInt col ++ " " ++ Parse.Error.contextToString context


addLineNumbers : String -> String
addLineNumbers s =
    let
        lines =
            String.split "\n" s

        largestLineNumberDecimalCount : Int
        largestLineNumberDecimalCount =
            String.length (String.fromInt (List.length lines))
    in
    String.join
        "\n"
        (List.indexedMap
            (\i line ->
                String.padLeft (largestLineNumberDecimalCount + 2)
                    ' '
                    (String.fromInt (i + 1) ++ "| ")
                    ++ line
            )
            lines
        )



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
