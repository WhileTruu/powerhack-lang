module Generate exposing (..)

import AST.Source as Source
import Data.Located as Located
import Data.VarName as VarName
import Parse.Declaration as Declaration exposing (Declaration)


generate : Declaration -> String
generate (Declaration.Value (Source.Value varName expr)) =
    [ "var " ++ VarName.toString (Located.unwrap varName) ++ " = " ++ generateExpr 0 expr
    ]
        |> String.join "\n"


indent : Int -> String -> String
indent lvl string =
    String.repeat (lvl * 4) " " ++ string


generateExpr : Int -> Source.LocatedExpr -> String
generateExpr lvl expr =
    case Located.unwrap expr of
        Source.Int int ->
            String.fromInt int

        Source.Call { fn, arguments } ->
            generateExpr lvl fn ++ "(" ++ String.join ", " (List.map (generateExpr lvl) arguments) ++ ")"

        Source.Var { name } ->
            VarName.toString name

        Source.Lambda { arguments, body } ->
            [ "function "
                ++ "("
                ++ String.join ", " (List.map VarName.toString arguments)
                ++ ") {"
            , indent (lvl + 1) ("return " ++ generateExpr (lvl + 1) body)
            , indent lvl "}"
            ]
                |> String.join "\n"

        Source.Defs defs expr_ ->
            [ "(function () {"
            , String.join "\n"
                (List.map
                    (\(Source.Define name defExpr) ->
                        indent (lvl + 1) ("var " ++ VarName.toString name ++ " = " ++ generateExpr lvl defExpr)
                    )
                    defs
                )
            , indent (lvl + 1) ("return " ++ generateExpr (lvl + 1) expr_)
            , indent lvl "})()"
            ]
                |> String.join "\n"

        Source.If { test, then_, else_ } ->
            [ "(function () {"
            , indent (lvl + 1) "if (" ++ generateExpr lvl test ++ ") {"
            , indent (lvl + 2) ("return " ++ generateExpr (lvl + 1) then_)
            , indent (lvl + 1) "} else {"
            , indent (lvl + 2) ("return " ++ generateExpr (lvl + 1) else_)
            , indent (lvl + 1) "}"
            , indent lvl "})()"
            ]
                |> String.join "\n"
