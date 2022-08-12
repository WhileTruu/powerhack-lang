module Emit.Js exposing (run)

import Data.Located as Located
import Data.Name as Name
import InferTypes


run : InferTypes.Module -> String
run module_ =
    [ "#!/usr/bin/env node"
    , builtIns
    , List.map generateJsValue module_.values
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


generateJsValue : InferTypes.Value -> String
generateJsValue (InferTypes.Value varName expr) =
    "var " ++ Name.toString (Located.toValue varName) ++ " = " ++ generateJsExpr 0 expr


indent : Int -> String -> String
indent lvl string =
    String.repeat (lvl * 4) " " ++ string


generateJsExpr : Int -> InferTypes.LocatedExpr -> String
generateJsExpr lvl expr =
    case Located.toValue expr of
        ( InferTypes.Int int, _ ) ->
            String.fromInt int

        ( InferTypes.Call fn argument, _ ) ->
            generateJsExpr lvl fn ++ "(" ++ generateJsExpr lvl argument ++ ")"

        ( InferTypes.Var name, _ ) ->
            Name.toString name

        ( InferTypes.Lambda argument body, _ ) ->
            [ "function (" ++ Name.toString argument ++ ") {"
            , indent (lvl + 1) ("return " ++ generateJsExpr (lvl + 1) body)
            , indent lvl "}"
            ]
                |> String.join "\n"

        ( InferTypes.Defs defs expr_, _ ) ->
            [ "(function () {"
            , String.join "\n"
                (List.map
                    (\(InferTypes.Define name defExpr) ->
                        indent (lvl + 1)
                            ("var " ++ Name.toString (Located.toValue name) ++ " = " ++ generateJsExpr lvl defExpr)
                    )
                    defs
                )
            , indent (lvl + 1) ("return " ++ generateJsExpr (lvl + 1) expr_)
            , indent lvl "})()"
            ]
                |> String.join "\n"

        ( InferTypes.If cond branch final, _ ) ->
            [ "(function () {"
            , indent (lvl + 1) "if (" ++ generateJsExpr lvl cond ++ ") {"
            , indent (lvl + 2) ("return " ++ generateJsExpr (lvl + 1) branch)
            , indent (lvl + 1) "} else {"
            , indent (lvl + 2) ("return " ++ generateJsExpr (lvl + 1) final)
            , indent (lvl + 1) "}"
            , indent lvl "})()"
            ]
                |> String.join "\n"
