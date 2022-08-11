module Emit exposing (Format(..), run)

import Emit.Js
import Emit.PrettyAST
import InferTypes


type Format
    = FormatJs
    | FormatPrettyAst


run : Format -> InferTypes.Module -> String
run format module_ =
    case format of
        FormatJs ->
            Emit.Js.run module_

        FormatPrettyAst ->
            Emit.PrettyAST.run module_
