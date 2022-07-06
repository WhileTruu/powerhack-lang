module Generate exposing (..)

import AST.Canonical as Canonical
import Data.Located as Located
import Data.VarName as VarName


generate : Canonical.Module -> String
generate module_ =
    List.map generateValue module_.values
        |> String.join "\n\n"


generateValue : Canonical.Value -> String
generateValue (Canonical.Value varName expr) =
    "var " ++ VarName.toString (Located.unwrap varName) ++ " = " ++ generateExpr 0 expr


indent : Int -> String -> String
indent lvl string =
    String.repeat (lvl * 4) " " ++ string


generateExpr : Int -> Canonical.LocatedExpr -> String
generateExpr lvl expr =
    case Located.unwrap expr of
        Canonical.Int int ->
            String.fromInt int

        Canonical.Call { fn, argument } ->
            generateExpr lvl fn ++ "(" ++ generateExpr lvl argument ++ ")"

        Canonical.Var { name } ->
            VarName.toString name

        Canonical.Lambda { argument, body } ->
            [ "function (" ++ VarName.toString argument ++ ") {"
            , indent (lvl + 1) ("return " ++ generateExpr (lvl + 1) body)
            , indent lvl "}"
            ]
                |> String.join "\n"

        Canonical.Defs defs expr_ ->
            [ "(function () {"
            , String.join "\n"
                (List.map
                    (\(Canonical.Define name defExpr) ->
                        indent (lvl + 1) ("var " ++ VarName.toString name ++ " = " ++ generateExpr lvl defExpr)
                    )
                    defs
                )
            , indent (lvl + 1) ("return " ++ generateExpr (lvl + 1) expr_)
            , indent lvl "})()"
            ]
                |> String.join "\n"

        Canonical.If { test, then_, else_ } ->
            [ "(function () {"
            , indent (lvl + 1) "if (" ++ generateExpr lvl test ++ ") {"
            , indent (lvl + 2) ("return " ++ generateExpr (lvl + 1) then_)
            , indent (lvl + 1) "} else {"
            , indent (lvl + 2) ("return " ++ generateExpr (lvl + 1) else_)
            , indent (lvl + 1) "}"
            , indent lvl "})()"
            ]
                |> String.join "\n"
