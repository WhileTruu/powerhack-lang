module Canonicalize exposing
    ( Error
    , canonicalizeExpr
    , run
    , toReport
    )

import AST.Canonical as Canonical
import AST.Source as Source
import AssocList as Dict exposing (Dict)
import Data.FilePath as FilePath
import Data.Located as Located
import Data.ModuleName as ModuleName exposing (ModuleName)
import Data.Name as Name exposing (Name)
import Report exposing (Report)


primitives : Dict Name Var
primitives =
    let
        dummyRegion : Located.Region
        dummyRegion =
            { start = Located.Position 0 0
            , end = Located.Position 0 0
            }
    in
    -- FIXME These need to match the builtins in Emit modules
    Dict.fromList
        [ ( Name.fromString "add", VarTopLevel (ModuleName.fromString "Primitives") dummyRegion )
        , ( Name.fromString "sub", VarTopLevel (ModuleName.fromString "Primitives") dummyRegion )
        , ( Name.fromString "eq", VarTopLevel (ModuleName.fromString "Primitives") dummyRegion )
        ]


run : Dict ModuleName Source.Module -> Result Error (Dict ModuleName Canonical.Module)
run source =
    Dict.foldl
        (\k v -> Result.map2 (Dict.insert k) (canonicalizeModule source k v))
        (Ok Dict.empty)
        source


canonicalizeModule :
    Dict ModuleName Source.Module
    -> ModuleName
    -> Source.Module
    -> Result Error Canonical.Module
canonicalizeModule modules name module_ =
    let
        vars : Dict Name Var
        vars =
            modules
                |> Dict.filter
                    (\k _ ->
                        (k == name)
                            || List.member k (Dict.keys module_.imports)
                    )
                |> Dict.foldl
                    (\k v acc ->
                        v.values
                            |> List.map
                                (\(Source.Value varName _) ->
                                    ( Located.toValue varName, VarTopLevel k (Located.getRegion varName) )
                                )
                            |> Dict.fromList
                            |> Dict.union acc
                    )
                    Dict.empty
                |> Dict.union primitives

        values : Result Error (List Canonical.Value)
        values =
            List.foldl
                (\(Source.Value varName expr) ->
                    Result.map2 ((::) << Canonical.Value varName)
                        (canonicalizeExpr { home = name, vars = vars }
                            expr
                        )
                )
                (Ok [])
                module_.values
    in
    Result.map (\a -> { imports = module_.imports, values = a }) values


type alias Env =
    { home : ModuleName
    , vars : Dict Name Var
    }


type Var
    = VarLocal
    | VarTopLevel ModuleName Located.Region


canonicalizeExpr : Env -> Source.LocatedExpr -> Result Error Canonical.LocatedExpr
canonicalizeExpr env sourceExpr =
    let
        region : Located.Region
        region =
            Located.getRegion sourceExpr
    in
    case Located.toValue sourceExpr of
        Source.Int int ->
            Ok (Located.replaceWith (Canonical.Int int) sourceExpr)

        Source.Call fn arguments ->
            let
                argsResults : List (Result Error Canonical.LocatedExpr)
                argsResults =
                    List.map (canonicalizeExpr env) arguments
            in
            List.foldl
                (Result.map2
                    (\arg acc ->
                        Located.located region (Canonical.Call acc arg)
                    )
                )
                (canonicalizeExpr env fn)
                argsResults

        Source.Var name ->
            findVar region env name

        Source.Lambda arguments body ->
            let
                newEnv : Env
                newEnv =
                    { env
                        | vars =
                            Dict.union
                                (Dict.fromList
                                    (List.map (\a -> ( a, VarLocal )) arguments)
                                )
                                env.vars
                    }
            in
            List.foldr (Result.map << (<<) (Located.located region) << Canonical.Lambda)
                (canonicalizeExpr newEnv body)
                arguments

        Source.Defs defs body ->
            let
                newEnv : Env
                newEnv =
                    { env
                        | vars =
                            Dict.union
                                (Dict.fromList
                                    (List.map
                                        (\(Source.Define varName _) ->
                                            ( Located.toValue varName
                                            , VarLocal
                                            )
                                        )
                                        defs
                                    )
                                )
                                env.vars
                    }

                canDefs : Result Error (List Canonical.Def)
                canDefs =
                    List.foldr
                        (\(Source.Define varName expr_) acc ->
                            Result.map2 ((::) << Canonical.Define varName)
                                (canonicalizeExpr newEnv expr_)
                                acc
                        )
                        (Ok [])
                        defs

                canBody : Result Error Canonical.LocatedExpr
                canBody =
                    canonicalizeExpr newEnv body
            in
            Result.map2 Canonical.Defs canDefs canBody
                |> Result.map (Located.located region)

        Source.If test then_ else_ ->
            Result.map3 Canonical.If
                (canonicalizeExpr env test)
                (canonicalizeExpr env then_)
                (canonicalizeExpr env else_)
                |> Result.map (Located.located region)


findVar : Located.Region -> Env -> Name -> Result Error Canonical.LocatedExpr
findVar region env name =
    case Dict.get name env.vars of
        Just var ->
            case var of
                VarLocal ->
                    Ok (Located.located region (Canonical.VarLocal name))

                VarTopLevel moduleName _ ->
                    Ok (Located.located region (Canonical.Var moduleName name))

        Nothing ->
            Err (ErrorNotFoundVar region name)



-- ERROR


type Error
    = ErrorNotFoundVar Located.Region Name


toReport : Error -> Report
toReport error =
    case error of
        ErrorNotFoundVar region name ->
            { filePath = FilePath.init "FIXME I don't know the file path 😢"
            , title = "NAMING ERROR"
            , message =
                [ "I cannot find a `" ++ Name.toString name ++ "` variable:"
                , (String.fromInt region.start.row ++ ":" ++ String.fromInt region.start.col)
                    ++ " - "
                    ++ (String.fromInt region.end.row ++ ":" ++ String.fromInt region.end.col)
                ]
                    |> String.join "\n\n"
            }
