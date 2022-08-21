module Canonical exposing
    ( Module, Value(..), LocatedExpr, Expr(..), Def(..)
    , Error, toReport
    , canonicalize
    )

{-| Canonical

@docs run


# AST

@docs Module, Value, LocatedExpr, Expr, Def


# Errors

@docs Error, toReport

-}

import AssocList as Dict exposing (Dict)
import Data.FilePath as FilePath
import Data.Located as Located exposing (Located)
import Data.ModuleName as ModuleName exposing (ModuleName)
import Data.Name as Name exposing (Name)
import Report exposing (Report)
import Source as Src


primitives : Dict Name VarKind
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
        [ ( Name.fromString "add", VarKindTopLevel (ModuleName.fromString "Primitives") dummyRegion )
        , ( Name.fromString "sub", VarKindTopLevel (ModuleName.fromString "Primitives") dummyRegion )
        , ( Name.fromString "eq", VarKindTopLevel (ModuleName.fromString "Primitives") dummyRegion )
        ]


canonicalize : Dict ModuleName Src.Module -> Result Error (Dict ModuleName Module)
canonicalize source =
    Dict.foldl
        (\k v -> Result.map2 (Dict.insert k) (canonicalizeModule source k v))
        (Ok Dict.empty)
        source


canonicalizeModule : Dict ModuleName Src.Module -> ModuleName -> Src.Module -> Result Error Module
canonicalizeModule modules name module_ =
    let
        vars : Dict Name VarKind
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
                                (\(Src.Value varName _) ->
                                    ( Located.toValue varName, VarKindTopLevel k (Located.getRegion varName) )
                                )
                            |> Dict.fromList
                            |> Dict.union acc
                    )
                    Dict.empty
                |> Dict.union primitives

        values : Result Error (List Value)
        values =
            List.foldl
                (\(Src.Value varName expr) ->
                    Result.map2 ((::) << Value varName)
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
    , vars : Dict Name VarKind
    }


type VarKind
    = VarKindLocal
    | VarKindTopLevel ModuleName Located.Region


canonicalizeExpr : Env -> Src.LocatedExpr -> Result Error LocatedExpr
canonicalizeExpr env sourceExpr =
    let
        region : Located.Region
        region =
            Located.getRegion sourceExpr
    in
    case Located.toValue sourceExpr of
        Src.Int int ->
            Ok (Located.replaceWith (Int int) sourceExpr)

        Src.Call fn arguments ->
            let
                argsResults : List (Result Error LocatedExpr)
                argsResults =
                    List.map (canonicalizeExpr env) arguments
            in
            List.foldl
                (Result.map2
                    (\arg acc ->
                        Located.located region (Call acc arg)
                    )
                )
                (canonicalizeExpr env fn)
                argsResults

        Src.Var name ->
            findVar region env name

        Src.Lambda arguments body ->
            let
                newEnv : Env
                newEnv =
                    { env
                        | vars =
                            Dict.union
                                (Dict.fromList
                                    (List.map (\a -> ( a, VarKindLocal )) arguments)
                                )
                                env.vars
                    }
            in
            List.foldr (Result.map << (<<) (Located.located region) << Lambda)
                (canonicalizeExpr newEnv body)
                arguments

        Src.Defs defs body ->
            let
                newEnv : Env
                newEnv =
                    { env
                        | vars =
                            Dict.union
                                (Dict.fromList
                                    (List.map
                                        (\(Src.Define varName _) ->
                                            ( Located.toValue varName
                                            , VarKindLocal
                                            )
                                        )
                                        defs
                                    )
                                )
                                env.vars
                    }

                canDefs : Result Error (List Def)
                canDefs =
                    List.foldr
                        (\(Src.Define varName expr_) acc ->
                            Result.map2 ((::) << Define varName)
                                (canonicalizeExpr newEnv expr_)
                                acc
                        )
                        (Ok [])
                        defs

                canBody : Result Error LocatedExpr
                canBody =
                    canonicalizeExpr newEnv body
            in
            Result.map2 Defs canDefs canBody
                |> Result.map (Located.located region)

        Src.If test then_ else_ ->
            Result.map3 If
                (canonicalizeExpr env test)
                (canonicalizeExpr env then_)
                (canonicalizeExpr env else_)
                |> Result.map (Located.located region)


findVar : Located.Region -> Env -> Name -> Result Error LocatedExpr
findVar region env name =
    case Dict.get name env.vars of
        Just var ->
            case var of
                VarKindLocal ->
                    Ok (Located.located region (VarLocal name))

                VarKindTopLevel moduleName _ ->
                    Ok (Located.located region (Var moduleName name))

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



-- AST EXPRESSIONS


type alias LocatedExpr =
    Located Expr


type Expr
    = Int Int
    | Call LocatedExpr LocatedExpr
    | Var ModuleName Name
    | VarLocal Name
    | Lambda Name LocatedExpr
    | Defs (List Def) LocatedExpr
    | If LocatedExpr LocatedExpr LocatedExpr



-- AST DEFINITIONS


type Def
    = Define (Located Name) LocatedExpr



-- AST MODULE


type alias Module =
    { imports : Dict ModuleName ()
    , values : List Value
    }


type Value
    = Value (Located Name) LocatedExpr
