module Emit exposing (Format(..), run)

import AssocList exposing (Dict)
import Data.ModuleName exposing (ModuleName)
import Emit.Js
import Emit.PrettyAST
import Typed


type Format
    = FormatJs
    | FormatPrettyAst


run : Format -> Dict ModuleName Typed.Module -> String
run format module_ =
    case format of
        FormatJs ->
            Emit.Js.run module_

        FormatPrettyAst ->
            Emit.PrettyAST.run module_
