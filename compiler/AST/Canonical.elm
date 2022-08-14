module AST.Canonical exposing
    ( Def(..)
    , Expr(..)
    , LocatedExpr
    , Module
    , Value(..)
    )

import AssocList exposing (Dict)
import Data.Located exposing (Located)
import Data.ModuleName exposing (ModuleName)
import Data.Name exposing (Name)


type alias LocatedExpr =
    Located Expr


type Expr
    = Int Int
    | Call LocatedExpr LocatedExpr
    | Var ModuleName Name
    | VarLocal Name
    | Lambda Name LocatedExpr
    | Defs (List Def) LocatedExpr
    | If LocatedExpr LocatedExpr LocatedExpr



-- DEFINITIONS


type Def
    = Define (Located Name) LocatedExpr



-- MODULE


type alias Module =
    { imports : Dict ModuleName ()
    , values : List Value
    }


type Value
    = Value (Located Name) LocatedExpr
