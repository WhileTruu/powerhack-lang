module Parse.Variable exposing (..)

import Data.VarName
import Parse.Error
import Parse.Parser exposing (Parser)
import Parser.Advanced as PA
import Set


variable : Parser Data.VarName.VarName
variable =
    PA.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "if", "then", "else" ]
        , expecting = Parse.Error.ExpectingVarName
        }
        |> PA.map Data.VarName.init
