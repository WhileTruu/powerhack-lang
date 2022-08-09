module AST.Canonical exposing
    ( Annotation(..)
    , Def(..)
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
    | Constructor Name Annotation
    | Call LocatedExpr LocatedExpr
    | Var Name
    | Lambda Name LocatedExpr
    | Defs (List Def) LocatedExpr
    | If LocatedExpr LocatedExpr LocatedExpr


type Annotation
    = Forall FreeVars Type


type FreeVars
    = Dict Name ()


type Type
    = TLambda Type Type
    | TType Name (List Type)
    | TVar Name



-- DEFINITIONS


type Def
    = Define (Located Name) LocatedExpr



-- MODULE


type alias Module =
    { values : List Value
    }


type Value
    = Value (Located Name) LocatedExpr
