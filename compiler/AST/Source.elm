module AST.Source exposing
    ( Def(..)
    , Expr(..)
    , LocatedExpr
    , Module
    , Value(..)
    )

import Data.Located exposing (Located)
import Data.Name exposing (Name)



-- EXPRESSIONS


type alias LocatedExpr =
    Located Expr


type Expr
    = Int Int
    | Call LocatedExpr (List LocatedExpr)
    | Var Name
    | Lambda (List Name) LocatedExpr
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
