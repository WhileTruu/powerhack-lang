module AST.Canonical exposing (..)

import Data.Located exposing (Located)
import Data.VarName exposing (VarName)


type alias LocatedExpr =
    Located Expr


type Expr
    = Int Int
    | Call { fn : LocatedExpr, argument : LocatedExpr }
    | Var { name : VarName }
    | Lambda { argument : VarName, body : LocatedExpr }
    | Defs (List Def) LocatedExpr
    | If { test : LocatedExpr, then_ : LocatedExpr, else_ : LocatedExpr }



-- DEFINITIONS


type Def
    = Define VarName LocatedExpr



-- MODULE


type alias Module =
    { values : List Value
    }


type Value
    = Value (Located VarName) LocatedExpr
