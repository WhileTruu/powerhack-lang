module Parse.Error exposing (Context(..), Problem(..), contextToString, problemToString)

import Data.FilePath as FilePath exposing (FilePath)


type Context
    = InDeclaration
    | InLambda
    | InDef
    | InDefs
    | InIf
    | InFile FilePath


contextToString : Context -> String
contextToString context =
    case context of
        InDeclaration ->
            "DECLARATION"

        InLambda ->
            "LAMBDA"

        InDef ->
            "DEFINITION"

        InDefs ->
            "DEFINITIONS"

        InIf ->
            "IF"

        InFile filePath ->
            "IN FILE: " ++ FilePath.toString filePath


type Problem
    = ExpectingVarName
    | ExpectingDef
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
    | ExpectingEnd


problemToString : Problem -> String
problemToString problem =
    case problem of
        ExpectingVarName ->
            "Expecting variable name"

        ExpectingDef ->
            "Expecting definition"

        InvalidTab ->
            "Invalid tab"

        InvalidNumber ->
            "Invalid number"

        ExpectingNumber ->
            "Expecting number"

        ExpectingOpenParen ->
            "Expecting open paren"

        ExpectingCloseParen ->
            "Expecting close paren"

        ExpectingComma ->
            "Expecting comma"

        ExpectingEquals ->
            "Expecting equals"

        ExpectingBackslash ->
            "Expecting backslash"

        ExpectingRightArrow ->
            "Expecting right arrow"

        FuncIdentBody ->
            "Expecting indentation in function body"

        ExpectingIndentation ->
            "Expecting indentation"

        ExpectingNoIndentation ->
            "Expecting no indentation"

        ExpectingIf ->
            "Expecting if"

        ExpectingThen ->
            "Expecting then"

        ExpectingElse ->
            "Expecting else"

        ExpectingEnd ->
            "Expecting end"
