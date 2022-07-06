module AST.Typed exposing (..)

-- EXPRESSIONS

import Data.Located exposing (Located)
import Data.Type exposing (TypeOrId)
import Data.VarName exposing (VarName)


type alias LocatedExpr =
    Located Expr


type alias Expr =
    ( Expr_, TypeOrId )


type Expr_
    = Int Int
    | Call { fn : LocatedExpr, arguments : List LocatedExpr }
    | Var { name : VarName }
    | Lambda { arguments : List VarName, body : LocatedExpr }
    | Defs (List Def) LocatedExpr
    | If { test : LocatedExpr, then_ : LocatedExpr, else_ : LocatedExpr }



-- DEFINITIONS


type Def
    = Define VarName LocatedExpr
