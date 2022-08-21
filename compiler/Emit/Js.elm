module Emit.Js exposing (run)

import AssocList as Dict exposing (Dict)
import Data.Located as Located
import Data.ModuleName exposing (ModuleName)
import Data.Name as Name
import Typed


run : Dict ModuleName Typed.Module -> String
run modules =
    [ "#!/usr/bin/env node"
    , builtIns
    , Dict.values modules
        |> List.concatMap (List.map generateJsValue << .values)
        |> String.join "\n\n"
    , "console.log((function () { return main() })())"
    ]
        |> String.join "\n\n"


builtIns : String
builtIns =
    -- FIXME These need to match the primitives in InferTypes module
    [ "var add = function (b) { return function (a) { return a + b; }; }"
    , "var sub = function (b) { return function (a) { return a - b; }; }"
    , "var eq = function (b) { return function (a) { return a === b; }; }"
    ]
        |> String.join "\n"


generateJsValue : Typed.Value -> String
generateJsValue (Typed.Value varName expr) =
    "var " ++ Name.toString (Located.toValue varName) ++ " = " ++ generateJsExpr 0 expr


indent : Int -> String -> String
indent lvl string =
    String.repeat (lvl * 4) " " ++ string


generateJsExpr : Int -> Typed.LocatedExpr -> String
generateJsExpr lvl expr =
    case Located.toValue expr of
        ( Typed.Int int, _ ) ->
            String.fromInt int

        ( Typed.Call fn argument, _ ) ->
            generateJsExpr lvl fn ++ "(" ++ generateJsExpr lvl argument ++ ")"

        ( Typed.Var _ name, _ ) ->
            -- FIXME no modulename
            Name.toString name

        ( Typed.VarLocal name, _ ) ->
            Name.toString name

        ( Typed.Lambda argument body, _ ) ->
            [ "function (" ++ Name.toString argument ++ ") {"
            , indent (lvl + 1) ("return " ++ generateJsExpr (lvl + 1) body)
            , indent lvl "}"
            ]
                |> String.join "\n"

        ( Typed.Defs defs expr_, _ ) ->
            [ "(function () {"
            , String.join "\n"
                (List.map
                    (\(Typed.Define name defExpr) ->
                        indent (lvl + 1)
                            ("var " ++ Name.toString (Located.toValue name) ++ " = " ++ generateJsExpr lvl defExpr)
                    )
                    defs
                )
            , indent (lvl + 1) ("return " ++ generateJsExpr (lvl + 1) expr_)
            , indent lvl "})()"
            ]
                |> String.join "\n"

        ( Typed.If cond branch final, _ ) ->
            [ "(function () {"
            , indent (lvl + 1) "if (" ++ generateJsExpr lvl cond ++ ") {"
            , indent (lvl + 2) ("return " ++ generateJsExpr (lvl + 1) branch)
            , indent (lvl + 1) "} else {"
            , indent (lvl + 2) ("return " ++ generateJsExpr (lvl + 1) final)
            , indent (lvl + 1) "}"
            , indent lvl "})()"
            ]
                |> String.join "\n"
