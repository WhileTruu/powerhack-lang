module Emit exposing (Format(..), run)

import AssocList exposing (Dict)
import Data.ModuleName exposing (ModuleName)
import Emit.Js
import Emit.PrettyAST
import InferTypes


type Format
    = FormatJs
    | FormatPrettyAst


run : Format -> Dict ModuleName InferTypes.Module -> String
run format module_ =
    case format of
        FormatJs ->
            Emit.Js.run module_

        FormatPrettyAst ->
            -- Emit.PrettyAST.run module_
            Debug.todo " "
