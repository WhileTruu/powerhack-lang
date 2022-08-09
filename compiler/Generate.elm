module Generate exposing (..)

import Data.Located as Located
import Data.Name as Name
import InferTypes


generate : InferTypes.Module -> String
generate module_ =
    List.map generateValue module_.values
        |> String.join "\n\n"


generateValue : InferTypes.Value -> String
generateValue (InferTypes.Value varName expr) =
    "var " ++ Name.toString (Located.toValue varName) ++ " = " ++ generateExpr 0 expr


indent : Int -> String -> String
indent lvl string =
    String.repeat (lvl * 4) " " ++ string


generateExpr : Int -> InferTypes.LocatedExpr -> String
generateExpr lvl expr =
    case Located.toValue expr of
        ( InferTypes.Int int, _ ) ->
            String.fromInt int

        ( InferTypes.Constructor name, _ ) ->
            -- FIXME no types atm
            Name.toString name

        ( InferTypes.Call fn argument, _ ) ->
            generateExpr lvl fn ++ "(" ++ generateExpr lvl argument ++ ")"

        ( InferTypes.Var name, _ ) ->
            Name.toString name

        ( InferTypes.Lambda argument body, _ ) ->
            [ "function (" ++ Name.toString argument ++ ") {"
            , indent (lvl + 1) ("return " ++ generateExpr (lvl + 1) body)
            , indent lvl "}"
            ]
                |> String.join "\n"

        ( InferTypes.Defs defs expr_, _ ) ->
            [ "(function () {"
            , String.join "\n"
                (List.map
                    (\(InferTypes.Define name defExpr) ->
                        indent (lvl + 1)
                            ("var " ++ Name.toString (Located.toValue name) ++ " = " ++ generateExpr lvl defExpr)
                    )
                    defs
                )
            , indent (lvl + 1) ("return " ++ generateExpr (lvl + 1) expr_)
            , indent lvl "})()"
            ]
                |> String.join "\n"

        ( InferTypes.If cond branch final, _ ) ->
            [ "(function () {"
            , indent (lvl + 1) "if (" ++ generateExpr lvl cond ++ ") {"
            , indent (lvl + 2) ("return " ++ generateExpr (lvl + 1) branch)
            , indent (lvl + 1) "} else {"
            , indent (lvl + 2) ("return " ++ generateExpr (lvl + 1) final)
            , indent (lvl + 1) "}"
            , indent lvl "})()"
            ]
                |> String.join "\n"
