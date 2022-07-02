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
import Data.VarName exposing (VarName)



-- EXPRESSIONS


type alias LocatedExpr =
    Located Expr


type Expr
    = Int Int
    | Call { fn : LocatedExpr, arguments : List LocatedExpr }
    | Var { name : VarName }
    | Lambda { arguments : List VarName, body : LocatedExpr }
    | Defs (List Def) LocatedExpr
    | If { test : LocatedExpr, then_ : LocatedExpr, else_ : LocatedExpr }



-- DEFINITIONS


type Def
    = Define VarName LocatedExpr


type alias LocatedPattern =
    Located Pattern



-- PATTERN


type Pattern
    = PatternVar VarName



-- TYPE


type alias LocatedType =
    Located Type


type Type
    = TLambda LocatedType LocatedType
    | TVar VarName
    | TType Located.Region VarName (List LocatedType)
    | TTypeQual Located.Region VarName VarName (List LocatedType)



-- MODULE


type alias Module =
    { values : List Value
    }


type Value
    = Value (Located VarName) LocatedExpr
