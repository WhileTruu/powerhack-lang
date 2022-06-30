module Parse.Error exposing (Context(..), Error(..), Problem(..), contextToString, problemToString)

import Data.FileContents exposing (FileContents)
import Parser.Advanced as PA


type Error
    = Problem ( List (PA.DeadEnd Context Problem), FileContents )


type Context
    = InDeclaration
    | InLambda
    | InDef
    | InDefs
    | InIf


contextToString : Context -> String
contextToString context =
    case context of
        InDeclaration ->
            "InDeclaration"

        InLambda ->
            "InLambda"

        InDef ->
            "InDef"

        InDefs ->
            "InDefs"

        InIf ->
            "InIf"


type Problem
    = ExpectingVarName
    | ExpectingDef
    | ExpectingEnd
    | InvalidTab
    | InvalidNumber
    | ExpectingNumber
    | ExpectingOpenParen
    | ExpectingCloseParen
    | ExpectingComma
    | ExpectingEquals
    | ExpectingBackslash
    | FuncIdentBody
    | ExpectingRightArrow
    | ExpectingIndentation
    | ExpectingNoIndentation
    | ExpectingIf
    | ExpectingThen
    | ExpectingElse


problemToString : Problem -> String
problemToString problem =
    case problem of
        ExpectingVarName ->
            "ExpectingVarName"

        ExpectingDef ->
            "ExpectingDef"

        ExpectingEnd ->
            "ExpectingEnd"

        InvalidTab ->
            "InvalidTab"

        InvalidNumber ->
            "InvalidNumber"

        ExpectingNumber ->
            "ExpectingNumber"

        ExpectingOpenParen ->
            "ExpectingOpenParen"

        ExpectingCloseParen ->
            "ExpectingCloseParen"

        ExpectingComma ->
            "ExpectingComma"

        ExpectingEquals ->
            "ExpectingEquals"

        ExpectingBackslash ->
            "ExpectingBackslash"

        ExpectingRightArrow ->
            "ExpectingRightArrow"

        FuncIdentBody ->
            "FuncIdentBody"

        ExpectingIndentation ->
            "ExpectingIndentation"

        ExpectingNoIndentation ->
            "ExpectingNoIndentation"

        ExpectingIf ->
            "ExpectingIf"

        ExpectingThen ->
            "ExpectingThen"

        ExpectingElse ->
            "ExpectingElse"
