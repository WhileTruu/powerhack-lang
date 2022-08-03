module Parse.Variable exposing (..)

import Data.Name as Name exposing (Name)
import Parse.Error as E
import Parser.Advanced as P
import Set


variable : P.Parser context E.Problem Name
variable =
    P.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "if", "then", "else" ]
        , expecting = E.ExpectingVarName
        }
        |> P.map Name.fromString
