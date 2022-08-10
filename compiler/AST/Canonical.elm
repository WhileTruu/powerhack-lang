module AST.Canonical exposing
    ( Def(..)
    , Expr(..)
    , LocatedExpr
    , Module
    , Value(..)
    )

import Data.Located exposing (Located)
import Data.Name exposing (Name)


type alias LocatedExpr =
    Located Expr


type Expr
    = Int Int
    | Call LocatedExpr LocatedExpr
    | Var Name
    | Lambda Name LocatedExpr
    | Defs (List Def) LocatedExpr
    | If LocatedExpr LocatedExpr LocatedExpr



-- DEFINITIONS


type Def
    = Define (Located Name) LocatedExpr



-- MODULE


type alias Module =
    { values : List Value
    }


type Value
    = Value (Located Name) LocatedExpr
