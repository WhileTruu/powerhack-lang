module Generate exposing (..)

import AST.Canonical as AST
import Data.Located as Located
import Data.Name as Name


generate : AST.Module -> String
generate module_ =
    List.map generateValue module_.values
        |> String.join "\n\n"


generateValue : AST.Value -> String
generateValue (AST.Value varName expr) =
    "var " ++ Name.toString (Located.unwrap varName) ++ " = " ++ generateExpr 0 expr


indent : Int -> String -> String
indent lvl string =
    String.repeat (lvl * 4) " " ++ string


generateExpr : Int -> AST.LocatedExpr -> String
generateExpr lvl expr =
    case Located.unwrap expr of
        AST.Int int ->
            String.fromInt int

        AST.Constructor name _ ->
            -- FIXME no types atm
            Name.toString name

        AST.Call fn argument ->
            generateExpr lvl fn ++ "(" ++ generateExpr lvl argument ++ ")"

        AST.Var name ->
            Name.toString name

        AST.Lambda argument body ->
            [ "function (" ++ Name.toString argument ++ ") {"
            , indent (lvl + 1) ("return " ++ generateExpr (lvl + 1) body)
            , indent lvl "}"
            ]
                |> String.join "\n"

        AST.Defs defs expr_ ->
            [ "(function () {"
            , String.join "\n"
                (List.map
                    (\(AST.Define name defExpr) ->
                        indent (lvl + 1) ("var " ++ Name.toString name ++ " = " ++ generateExpr lvl defExpr)
                    )
                    defs
                )
            , indent (lvl + 1) ("return " ++ generateExpr (lvl + 1) expr_)
            , indent lvl "})()"
            ]
                |> String.join "\n"

        AST.If test then_ else_ ->
            [ "(function () {"
            , indent (lvl + 1) "if (" ++ generateExpr lvl test ++ ") {"
            , indent (lvl + 2) ("return " ++ generateExpr (lvl + 1) then_)
            , indent (lvl + 1) "} else {"
            , indent (lvl + 2) ("return " ++ generateExpr (lvl + 1) else_)
            , indent (lvl + 1) "}"
            , indent lvl "})()"
            ]
                |> String.join "\n"
