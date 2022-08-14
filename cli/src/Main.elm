module Main exposing (program)

import AST.Canonical as Can
import AST.Source as Source
import AssocList as Dict exposing (Dict)
import Canonicalize
import Data.FileContents as FileContents
import Data.FilePath as FilePath
import Data.ModuleName as ModuleName exposing (ModuleName)
import Emit
import Emit.PrettyAST
import Error
import InferTypes
import List.Extra as List
import Parse
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
        |> Maybe.withDefault (IO.fail InvalidArgsError)
        |> IO.and (IO.exit 0)
        |> IO.recover (IO.print << prettyError)
        |> IO.and (IO.exit 1)


type Error
    = FileReadError String
    | FileWriteError String
    | ParseError Parse.Error
    | CanonicalizationError Canonicalize.Error
    | TypeError InferTypes.SuperError
    | InvalidArgsError


prettyError : Error -> String
prettyError err =
    case err of
        FileReadError fileReadError ->
            Debug.toString fileReadError

        FileWriteError fileWriteError ->
            Debug.toString fileWriteError

        ParseError parseError ->
            Debug.toString parseError

        CanonicalizationError canError ->
            Debug.toString canError

        TypeError { errors, subst, modules } ->
            [ "Type errors: " ++ Debug.toString errors
            , Emit.PrettyAST.run modules
            , InferTypes.prettySubst subst
            ]
                |> String.join "\n\n\n"

        InvalidArgsError ->
            Debug.toString InvalidArgsError


mainModuleNameFromFileName : IOFile.Filename -> ModuleName
mainModuleNameFromFileName fileName =
    -- FIXME unsafe
    String.split "/" fileName
        |> List.last
        |> Maybe.map (String.split ".")
        |> Maybe.andThen List.head
        |> Maybe.withDefault "Main"
        |> ModuleName.fromString


sourcePathFromMainFileName : IOFile.Filename -> String
sourcePathFromMainFileName fileName =
    String.split "/" fileName
        |> List.reverse
        |> List.drop 1
        |> List.reverse
        |> String.join "/"


readAndParseModules : IOFile.Filename -> IO Error (Dict ModuleName Source.Module)
readAndParseModules fileName =
    let
        mainModuleName : ModuleName
        mainModuleName =
            mainModuleNameFromFileName fileName

        sourcePath : String
        sourcePath =
            sourcePathFromMainFileName fileName
    in
    IOFile.read fileName
        |> IO.mapError FileReadError
        |> IO.andThen
            (\contents ->
                Parse.run (FilePath.init fileName) (FileContents.init contents)
                    |> Result.mapError ParseError
                    |> IO.fromResult
            )
        |> IO.andThen
            (\mainModule ->
                let
                    imports : List ModuleName
                    imports =
                        Dict.keys mainModule.imports
                in
                imports
                    |> List.map (\import_ -> sourcePath ++ "/" ++ ModuleName.toString import_ ++ ".powerhack")
                    |> List.map readAndParseModules
                    |> IO.combine
                    |> IO.map (List.foldl Dict.union (Dict.singleton mainModuleName mainModule))
            )


compile : Dict ModuleName Source.Module -> Result Error String
compile sourceModules =
    Canonicalize.run sourceModules
        |> Result.mapError CanonicalizationError
        |> Result.andThen
            (InferTypes.runHarder
                >> Result.mapError TypeError
            )
        |> Result.map (Emit.run Emit.FormatJs << Tuple.first)


readCompileAndWrite : { a | file : IOFile.Filename, output : String } -> IO Error String
readCompileAndWrite { file, output } =
    readAndParseModules file
        |> IO.andThen
            (\sourceModules ->
                let
                    result : Result Error String
                    result =
                        Canonicalize.run sourceModules
                            |> Result.mapError CanonicalizationError
                            |> Result.andThen
                                (InferTypes.runHarder
                                    >> Result.mapError TypeError
                                )
                            |> Result.map (Emit.run Emit.FormatJs << Tuple.first)
                in
                case result of
                    Ok outputS ->
                        [ "Success! Compiled "
                            ++ String.fromInt (Dict.size sourceModules)
                            ++ (if Dict.size sourceModules > 1 then
                                    " modules."

                                else
                                    "module."
                               )
                        , ""
                        , "    " ++ file ++ " ───> " ++ output
                        , ""
                        , ""
                        ]
                            |> String.join "\n"
                            |> IO.print
                            |> IO.and
                                (IOFile.write
                                    (IOFile.CreateIfNotExists IOFile.Truncate IOFilePermission.default)
                                    output
                                    outputS
                                )
                            |> IO.mapError FileWriteError
                            |> IO.and (IO.return output)

                    Err error ->
                        IO.fail error
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
