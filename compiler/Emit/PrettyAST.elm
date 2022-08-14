module Emit.PrettyAST exposing (run)

-- PRETTY AST

import AssocList as Dict exposing (Dict)
import Console
import Data.Located as Located
import Data.ModuleName as ModuleName exposing (ModuleName)
import Data.Name as Name
import InferTypes


run : Dict ModuleName InferTypes.Module -> String
run modules =
    Dict.toList modules
        |> List.concatMap (\( a, b ) -> List.map (generatePrettyAstValue a) b.values)
        |> String.join "\n\n"


generatePrettyAstValue : ModuleName -> InferTypes.Value -> String
generatePrettyAstValue moduleName (InferTypes.Value varName expr) =
    [ Console.cyan "def: " ++ Console.green (InferTypes.prettyType (typeFromExpr expr))
    , indent 1 ("module: " ++ Console.red (ModuleName.toString moduleName))
    , indent 1 ("name: " ++ Console.red (Name.toString (Located.toValue varName)))
    , indent 1 "body:"
    , generatePrettyAstExpr 2 expr
    ]
        |> String.join "\n"


generatePrettyAstExpr : Int -> InferTypes.LocatedExpr -> String
generatePrettyAstExpr lvl expr =
    case Located.toValue expr of
        ( InferTypes.Int int, type_ ) ->
            indent lvl
                (Console.cyan "int: "
                    ++ Console.green (InferTypes.prettyType type_)
                    ++ Console.blue (" (value: " ++ String.fromInt int ++ ")")
                )

        ( InferTypes.Call fn argument, type_ ) ->
            [ indent lvl (Console.cyan "call: " ++ Console.green (InferTypes.prettyType type_))
            , indent (lvl + 1) "fn:"
            , generatePrettyAstExpr (lvl + 2) fn
            , indent (lvl + 1) "arg:"
            , generatePrettyAstExpr (lvl + 2) argument
            ]
                |> String.join "\n"

        ( InferTypes.Var moduleName name, type_ ) ->
            [ indent lvl (Console.cyan "var: ") ++ Console.green (InferTypes.prettyType type_)
            , indent (lvl + 1) ("module: " ++ Console.red (ModuleName.toString moduleName))
            , indent (lvl + 1) ("name: " ++ Console.red (Name.toString name))
            ]
                |> String.join "\n"

        ( InferTypes.VarLocal name, type_ ) ->
            [ indent lvl (Console.cyan "var_local: ") ++ Console.green (InferTypes.prettyType type_)
            , indent (lvl + 1) ("name: " ++ Console.red (Name.toString name))
            ]
                |> String.join "\n"

        ( InferTypes.Lambda argument body, type_ ) ->
            [ indent lvl (Console.cyan "lambda: ") ++ Console.green (InferTypes.prettyType type_)
            , indent (lvl + 1) "arg:"
            , indent (lvl + 2) "name: " ++ Console.red (Name.toString argument)
            , indent (lvl + 1) "body:"
            , generatePrettyAstExpr (lvl + 2) body
            ]
                |> String.join "\n"

        ( InferTypes.Defs defs expr_, type_ ) ->
            [ indent lvl (Console.cyan "defs: ") ++ Console.green (InferTypes.prettyType type_)
            , String.join "\n"
                (List.map
                    (\(InferTypes.Define name defExpr) ->
                        [ indent (lvl + 1) (Console.cyan "def: ") ++ Console.green (InferTypes.prettyType (typeFromExpr defExpr))
                        , indent (lvl + 2) ("name: " ++ Console.red (Name.toString (Located.toValue name)))
                        , indent (lvl + 2) "body:"
                        , generatePrettyAstExpr (lvl + 3) defExpr
                        ]
                            |> String.join "\n"
                    )
                    defs
                )
            , indent (lvl + 1) "body: "
            , generatePrettyAstExpr (lvl + 2) expr_
            ]
                |> String.join "\n"

        ( InferTypes.If cond branch final, type_ ) ->
            [ indent lvl (Console.cyan "if: ") ++ Console.green (InferTypes.prettyType type_)
            , indent (lvl + 1) "cond:"
            , generatePrettyAstExpr (lvl + 2) cond
            , indent (lvl + 1) "branch:"
            , generatePrettyAstExpr (lvl + 2) branch
            , indent (lvl + 1) "final:"
            , generatePrettyAstExpr (lvl + 2) final
            ]
                |> String.join "\n"


typeFromExpr : InferTypes.LocatedExpr -> InferTypes.Type
typeFromExpr locatedExpr =
    Located.toValue locatedExpr
        |> (\( _, type_ ) -> type_)


indent : Int -> String -> String
indent lvl string =
    String.repeat lvl " " ++ string
