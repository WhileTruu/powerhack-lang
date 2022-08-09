module AST.Source exposing
    ( Def(..)
    , Expr(..)
    , LocatedExpr
    , LocatedPattern
    , Module
    , Pattern(..)
    , Value(..)
    )

import Data.Located as Located exposing (Located)
import Data.Name exposing (Name)



-- EXPRESSIONS


type alias LocatedExpr =
    Located Expr


type Expr
    = Int Int
    | Constructor Name
    | Call LocatedExpr (List LocatedExpr)
    | Var Name
    | Lambda (List Name) LocatedExpr
    | Defs (List Def) LocatedExpr
    | If LocatedExpr LocatedExpr LocatedExpr



-- DEFINITIONS


type Def
    = Define (Located Name) LocatedExpr



-- PATTERN


type alias LocatedPattern =
    Located Pattern


type Pattern
    = PatternVar Name



-- TYPE


type alias LocatedType =
    Located Type


type Type
    = TLambda LocatedType LocatedType
    | TVar Name
    | TType Located.Region Name (List LocatedType)
    | TTypeQual Located.Region Name Name (List LocatedType)



-- MODULE


type alias Module =
    { values : List Value
    }


type Value
    = Value (Located Name) LocatedExpr
