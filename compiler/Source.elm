module Source exposing
    ( parse
    , Module, Value(..), LocatedExpr, Expr(..), Def(..)
    , Error(..), toReport
    , testSuite
    )

{-| Source

@docs parse


# AST

@docs Module, Value, LocatedExpr, Expr, Def


# Errors

@docs Error, toReport


# Tests

@docs testSuite

-}

import AssocList as Dict exposing (Dict)
import Console
import Data.FileContents as FileContents exposing (FileContents)
import Data.FilePath as FilePath exposing (FilePath)
import Data.Located as Located exposing (Located)
import Data.ModuleName as ModuleName exposing (ModuleName)
import Data.Name as Name exposing (Name)
import Expect
import Parser.Advanced as P exposing ((|.), (|=))
import Report exposing (Report)
import Set
import String.Extra
import Test exposing (Test)


parse : FilePath -> FileContents -> Result (List Error) Module
parse filePath fileContents =
    P.run module_ (FileContents.toString fileContents)
        |> Result.mapError (List.map (Error filePath))


reserved : Set.Set String
reserved =
    Set.fromList
        [ "if"
        , "then"
        , "else"
        , "imports"
        ]



-- MODULE


module_ : P.Parser Context Problem Module
module_ =
    P.succeed Module
        |= imports
        |. ignoreables
        |= (oneOrMoreWith ignoreables declaration
                |> P.map (\( decl, decls ) -> categorizeDecls (decl :: decls))
           )
        |. P.end ExpectingEnd


categorizeDecls : List Declaration -> List Value
categorizeDecls decls =
    List.foldl
        (\decl values ->
            case decl of
                DeclarationValue val ->
                    val :: values
        )
        []
        decls



-- IMPORTS


imports : P.Parser Context Problem (Dict ModuleName ())
imports =
    P.oneOf
        [ P.succeed identity
            |. P.keyword (P.Token "imports" ExpectingImports)
            |. ignoreables
            |= (P.sequence
                    { start = P.Token "[" ExpectingOpenBracket
                    , separator = P.Token "," ExpectingComma
                    , end = P.Token "]" ExpectingCloseBracket
                    , spaces = ignoreables
                    , item = moduleName
                    , trailing = P.Optional
                    }
                    |> P.map (Dict.fromList << List.map (\a -> ( a, () )))
               )
        , P.succeed Dict.empty
        ]


moduleName : P.Parser context Problem ModuleName
moduleName =
    P.variable
        { start = Char.isUpper
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = reserved
        , expecting = ExpectingModuleName
        }
        |> P.map ModuleName.fromString



-- DECLARATION


type Declaration
    = DeclarationValue Value


declaration : P.Parser Context Problem Declaration
declaration =
    P.succeed (\name body -> DeclarationValue (Value name body))
        |= onlyAtBeginningOfLine
            (variable
                |> located
            )
        |. ignoreables
        |= P.oneOf
            [ P.succeed identity
                |. notAtBeginningOfLine (P.symbol (P.Token ":" ExpectingEquals))
                |. ignoreables
                |= P.lazy (\_ -> expression)
            , P.succeed identity
                |. notAtBeginningOfLine (P.symbol (P.Token "=" ExpectingEquals))
                |. ignoreables
                |= P.lazy (\_ -> expression)
            ]



-- EXPRESSION


term : P.Parser Context Problem LocatedExpr
term =
    P.oneOf
        [ variable
            |> P.map Var
            |> located
        , P.number
            { int = Result.Ok Int
            , hex = Result.Err InvalidNumber
            , octal = Result.Err InvalidNumber
            , binary = Result.Err InvalidNumber
            , float = Result.Err InvalidNumber
            , invalid = InvalidNumber
            , expecting = ExpectingNumber
            }
            |> located
            -- FIXME remove backtrackable
            |> P.backtrackable
        , P.succeed identity
            |. P.symbol (P.Token "(" ExpectingOpenParen)
            |= P.lazy (\_ -> expression)
            |. P.symbol (P.Token ")" ExpectingCloseParen)
        ]


expression : P.Parser Context Problem LocatedExpr
expression =
    P.oneOf
        [ if_
        , lambda
        , rememberIndentation defsOrVarAndChompExprEnd
        , term
            |> P.andThen
                (\expr ->
                    chompExprEnd expr
                )
        ]


chompExprEnd : LocatedExpr -> P.Parser Context Problem LocatedExpr
chompExprEnd arg =
    P.loop [] (chompExprEndHelp arg)


chompExprEndHelp :
    LocatedExpr
    -> List LocatedExpr
    -> P.Parser Context Problem (P.Step (List LocatedExpr) LocatedExpr)
chompExprEndHelp arg vs =
    P.succeed identity
        |. ignoreables
        |= P.oneOf
            [ P.succeed identity
                |. checkIndent (<) ExpectingIndentation
                |= term
                |> P.map (\v -> P.Loop (v :: vs))
            , if List.isEmpty vs then
                P.succeed (P.Done arg)

              else
                P.succeed (Call arg (List.reverse vs))
                    |> located
                    |> P.map P.Done
            ]


defsOrVarAndChompExprEnd : P.Parser Context Problem LocatedExpr
defsOrVarAndChompExprEnd =
    P.loop [] defsOrVarAndChompExprEndHelp


defsOrVarAndChompExprEndHelp :
    List Def
    -> P.Parser Context Problem (P.Step (List Def) LocatedExpr)
defsOrVarAndChompExprEndHelp args =
    P.oneOf
        [ P.succeed identity
            |= (variable
                    |> located
               )
            |. spacesOnly
            |> P.andThen
                (\name ->
                    P.oneOf
                        [ P.succeed (\body -> Define name body)
                            |. P.symbol (P.Token "=" ExpectingEquals)
                            |. ignoreablesAndCheckIndent (<) ExpectingIndentation
                            |= P.lazy (\_ -> expression)
                            |> P.inContext InDef
                            |> P.map (\def2_ -> P.Loop (def2_ :: args))
                        , if List.isEmpty args then
                            varAndChompExprEnd name
                                |> P.map P.Done

                          else
                            P.succeed (\expr -> Defs (List.reverse args) expr)
                                |= varAndChompExprEnd name
                                |> P.inContext InDefs
                                |> located
                                |> P.map P.Done
                        ]
                )
        , if List.isEmpty args then
            P.problem ExpectingDef

          else
            P.succeed (\expr -> Defs (List.reverse args) expr)
                |= P.lazy (\_ -> expression)
                |> P.inContext InDefs
                |> located
                |> P.map P.Done
        ]


varAndChompExprEnd : Located Name -> P.Parser Context Problem LocatedExpr
varAndChompExprEnd name =
    P.succeed (Var (Located.toValue name))
        |> located
        |> P.andThen (\expr -> chompExprEnd expr)


lambda : P.Parser Context Problem LocatedExpr
lambda =
    P.succeed (\( arg, args ) body -> Lambda (arg :: args) body)
        |. backslash
        |= oneOrMoreWith spacesOnly variable
        |. spacesOnly
        |. P.symbol (P.Token "->" ExpectingRightArrow)
        |. ignoreablesAndCheckIndent (<) FuncIdentBody
        |= P.lazy (\_ -> expression)
        |> P.inContext InLambda
        |> located


if_ : P.Parser Context Problem LocatedExpr
if_ =
    P.succeed If
        |. P.keyword (P.Token "if" ExpectingIf)
        |. ignoreables
        |= P.lazy (\_ -> expression)
        |. ignoreables
        |. P.keyword (P.Token "then" ExpectingThen)
        |. ignoreables
        |= P.lazy (\_ -> expression)
        |. ignoreables
        |. P.keyword (P.Token "else" ExpectingElse)
        |. ignoreables
        |= P.lazy (\_ -> expression)
        |> P.inContext InIf
        |> located



-- VARIABLE


variable : P.Parser context Problem Name
variable =
    P.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = reserved
        , expecting = ExpectingVarName
        }
        |> P.map Name.fromString



-- HELPERS


located : P.Parser context problem p -> P.Parser context problem (Located p)
located p =
    P.succeed
        (\( startRow, startCol ) value ( endRow, endCol ) ->
            Located.located
                { start = { row = startRow, col = startCol }
                , end = { row = endRow, col = endCol }
                }
                value
        )
        |= P.getPosition
        |= p
        |= P.getPosition


ignoreables : P.Parser context Problem ()
ignoreables =
    P.loop 0 <|
        ifProgress <|
            P.oneOf
                [ P.symbol (P.Token "\t" InvalidTab)
                    |> P.andThen (\_ -> P.problem InvalidTab)
                , P.spaces
                ]


ifProgress : P.Parser context problem a -> Int -> P.Parser context problem (P.Step Int ())
ifProgress parser offset =
    P.succeed identity
        |. parser
        |= P.getOffset
        |> P.map
            (\newOffset ->
                if offset == newOffset then
                    P.Done ()

                else
                    P.Loop newOffset
            )


backslash : P.Parser context Problem ()
backslash =
    P.token (P.Token "\\" ExpectingBackslash)


spacesOnly : P.Parser context problem ()
spacesOnly =
    P.chompWhile ((==) ' ')


oneOrMoreWith :
    P.Parser context problem ()
    -> P.Parser context problem a
    -> P.Parser context problem ( a, List a )
oneOrMoreWith spaces p =
    P.succeed Tuple.pair
        |= p
        |. spaces
        |= P.loop [] (oneOrMoreHelp spaces p)


oneOrMoreHelp :
    P.Parser context problem ()
    -> P.Parser context problem a
    -> List a
    -> P.Parser context problem (P.Step (List a) (List a))
oneOrMoreHelp spaces p vs =
    P.oneOf
        [ P.succeed (\v -> P.Loop (v :: vs))
            |= p
            |. spaces
        , P.succeed ()
            |> P.map (always (P.Done (List.reverse vs)))
        ]


ignoreablesAndCheckIndent : (Int -> Int -> Bool) -> Problem -> P.Parser context Problem ()
ignoreablesAndCheckIndent check error =
    P.succeed ()
        |. ignoreables
        |. checkIndent check error


checkIndent : (Int -> Int -> Bool) -> problem -> P.Parser context problem ()
checkIndent check error =
    P.succeed
        (\indent col ->
            if check indent col then
                P.succeed ()

            else
                P.problem error
        )
        |= P.getIndent
        |= P.getCol
        |> P.andThen identity


notAtBeginningOfLine : P.Parser context Problem a -> P.Parser context Problem a
notAtBeginningOfLine parser =
    P.succeed identity
        |. checkIndent (\_ column -> column > 1) ExpectingIndentation
        |= parser


onlyAtBeginningOfLine : P.Parser context Problem a -> P.Parser context Problem a
onlyAtBeginningOfLine parser =
    P.succeed identity
        |. checkIndent (\_ column -> column == 1) ExpectingNoIndentation
        |= parser


rememberIndentation : P.Parser context problem a -> P.Parser context problem a
rememberIndentation parser =
    P.getCol
        |> P.andThen (\col -> P.withIndent col parser)



-- ERROR


type Error
    = Error FilePath (P.DeadEnd Context Problem)


type Context
    = InLambda
    | InDef
    | InDefs
    | InIf


type Problem
    = ExpectingVarName
    | ExpectingDef
    | InvalidTab
    | InvalidNumber
    | ExpectingNumber
    | ExpectingOpenParen
    | ExpectingCloseParen
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
    | ExpectingImports
    | ExpectingOpenBracket
    | ExpectingComma
    | ExpectingCloseBracket
    | ExpectingModuleName


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

        ExpectingImports ->
            "Expecting imports"

        ExpectingOpenBracket ->
            "Expecting open bracket"

        ExpectingComma ->
            "Expecting comma"

        ExpectingCloseBracket ->
            "Expecting close bracket"

        ExpectingModuleName ->
            "Expecting module name"


toReport : FileContents -> Error -> Report
toReport fileContents (Error filePath deadEnd) =
    { filePath = filePath
    , title =
        case deadEnd.contextStack of
            [] ->
                "PROBLEM WITH PARSING?"

            first :: _ ->
                "PROBLEM " ++ contextToString first.context
    , message =
        [ case deadEnd.contextStack of
            [] ->
                "I got stuck here:"

            first :: _ ->
                case first.context of
                    InLambda ->
                        "I got stuck parsing this lambda here:"

                    InDef ->
                        "I got stuck parsing this definition here:"

                    InDefs ->
                        "I got stuck parsing these definitions here:"

                    InIf ->
                        "I got stuck parsing this `if` expression here:"
        , Report.renderErrorInFileContents fileContents
            { startRow =
                List.head deadEnd.contextStack
                    |> Maybe.map .row
                    |> Maybe.withDefault deadEnd.row
            , row = deadEnd.row
            , col = deadEnd.col
            }
        , case deadEnd.problem of
            ExpectingVarName ->
                "I was expecting to see a variable name."

            ExpectingDef ->
                "I was expecting to see a def."

            InvalidTab ->
                "FIXME: I don't like tabs."

            InvalidNumber ->
                "I was expecting to see a valid number."

            ExpectingNumber ->
                "I was expecting to see a number."

            ExpectingOpenParen ->
                "I was expecting to see an opening parenthesis next. Try putting a "
                    ++ Console.green "("
                    ++ " next and see if that helps?"

            ExpectingCloseParen ->
                "I was expecting to see a closing parenthesis next. Try putting a "
                    ++ Console.green ")"
                    ++ " next and see if that helps?"

            ExpectingEquals ->
                "I was expecting to see the equals sign (" ++ Console.green "=" ++ ") next."

            ExpectingBackslash ->
                "I was expecting to see a backslash next."

            ExpectingRightArrow ->
                "I was expecting to see the right arrow " ++ Console.green "->" ++ " next."

            FuncIdentBody ->
                "I was expecting the function body to be indented."

            ExpectingIndentation ->
                "I was expecting to see indentation next."

            ExpectingNoIndentation ->
                "I was expecting to see no indentation  next."

            ExpectingIf ->
                "I was expecting to see the " ++ Console.green "if" ++ " keyword next."

            ExpectingThen ->
                "I was expecting to see the " ++ Console.green "then" ++ " keyword next."

            ExpectingElse ->
                "I was expecting to see the " ++ Console.green "else" ++ " keyword next."

            ExpectingEnd ->
                "Whatever is here, I wasn't expecting it!"

            ExpectingImports ->
                "I was expecting to see the " ++ Console.green "imports" ++ " keyword next."

            ExpectingOpenBracket ->
                "I was expecting to see the open bracket (" ++ Console.green "[" ++ ") next."

            ExpectingComma ->
                "I was expecting to see the comma sign (" ++ Console.green "," ++ ") next."

            ExpectingCloseBracket ->
                "I was expecting to see the close bracket (" ++ Console.green "]" ++ ") next."

            ExpectingModuleName ->
                -- TODO Add better explanation
                "I was expecting to see the module name next."
                    ++ "Module names start with an uppercase letter and can only "
                    ++ "contain letters, numbers, and underscores."
        , deadEndToString deadEnd
        ]
            |> String.join "\n\n"
    }


deadEndToString : P.DeadEnd Context Problem -> String
deadEndToString { row, col, problem, contextStack } =
    String.fromInt row
        ++ ":"
        ++ String.fromInt col
        ++ " "
        ++ problemToString problem
        ++ " <--\n    "
        ++ String.join " <-- " (List.map locatedContextToString contextStack)


locatedContextToString : { row : Int, col : Int, context : Context } -> String
locatedContextToString { row, col, context } =
    String.fromInt row ++ ":" ++ String.fromInt col ++ " " ++ contextToString context


contextToString : Context -> String
contextToString context =
    case context of
        InLambda ->
            "LAMBDA"

        InDef ->
            "DEFINITION"

        InDefs ->
            "DEFINITIONS"

        InIf ->
            "IF"



-- TESTS


testSuite : Test
testSuite =
    let
        parseString : String -> Result (List Error) (List Value)
        parseString input =
            parse (FilePath.init "Test.powerhack") (FileContents.init input)
                |> Result.map .values
    in
    Test.describe "Suite"
        [ Test.test "parse succeeds for lambda with if else and call" <|
            \_ ->
                """
                main = \\arggg ->
                    fib 10 0 1

                fib = \\n a b ->
                    if eq 0 n then
                        a
                    else
                        fib (sub 1 n) b (add b a)
                """
                    |> (String.Extra.unindent >> String.trim)
                    |> parseString
                    |> Expect.equal
                        (Ok
                            [ Value
                                (Located.located { end = { col = 4, row = 4 }, start = { col = 1, row = 4 } }
                                    (Name.fromString "fib")
                                )
                                (Located.located { end = { col = 34, row = 8 }, start = { col = 7, row = 4 } }
                                    (Lambda [ Name.fromString "n", Name.fromString "a", Name.fromString "b" ]
                                        (Located.located { end = { col = 34, row = 8 }, start = { col = 5, row = 5 } }
                                            (If
                                                (Located.located { end = { col = 15, row = 5 }, start = { col = 15, row = 5 } }
                                                    (Call
                                                        (Located.located { end = { col = 11, row = 5 }, start = { col = 11, row = 5 } }
                                                            (Var (Name.fromString "eq"))
                                                        )
                                                        [ Located.located { end = { col = 12, row = 5 }, start = { col = 11, row = 5 } }
                                                            (Int 0)
                                                        , Located.located { end = { col = 14, row = 5 }, start = { col = 13, row = 5 } }
                                                            (Var (Name.fromString "n"))
                                                        ]
                                                    )
                                                )
                                                (Located.located { end = { col = 10, row = 6 }, start = { col = 10, row = 6 } }
                                                    (Var (Name.fromString "a"))
                                                )
                                                (Located.located { end = { col = 34, row = 8 }, start = { col = 34, row = 8 } }
                                                    (Call
                                                        (Located.located { end = { col = 13, row = 8 }, start = { col = 13, row = 8 } }
                                                            (Var (Name.fromString "fib"))
                                                        )
                                                        [ Located.located { end = { col = 21, row = 8 }, start = { col = 21, row = 8 } }
                                                            (Call
                                                                (Located.located { end = { col = 18, row = 8 }, start = { col = 18, row = 8 } }
                                                                    (Var (Name.fromString "sub"))
                                                                )
                                                                [ Located.located { end = { col = 19, row = 8 }, start = { col = 18, row = 8 } }
                                                                    (Int 1)
                                                                , Located.located { end = { col = 21, row = 8 }, start = { col = 20, row = 8 } }
                                                                    (Var (Name.fromString "n"))
                                                                ]
                                                            )
                                                        , Located.located { end = { col = 24, row = 8 }, start = { col = 23, row = 8 } }
                                                            (Var (Name.fromString "b"))
                                                        , Located.located { end = { col = 33, row = 8 }, start = { col = 33, row = 8 } }
                                                            (Call
                                                                (Located.located { end = { col = 30, row = 8 }, start = { col = 30, row = 8 } }
                                                                    (Var (Name.fromString "add"))
                                                                )
                                                                [ Located.located { end = { col = 31, row = 8 }, start = { col = 30, row = 8 } }
                                                                    (Var (Name.fromString "b"))
                                                                , Located.located { end = { col = 33, row = 8 }, start = { col = 32, row = 8 } }
                                                                    (Var (Name.fromString "a"))
                                                                ]
                                                            )
                                                        ]
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            , Value
                                (Located.located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } }
                                    (Name.fromString "main")
                                )
                                (Located.located { end = { col = 1, row = 4 }, start = { col = 8, row = 1 } }
                                    (Lambda [ Name.fromString "arggg" ]
                                        (Located.located { end = { col = 1, row = 4 }, start = { col = 1, row = 4 } }
                                            (Call
                                                (Located.located { end = { col = 9, row = 2 }, start = { col = 9, row = 2 } }
                                                    (Var (Name.fromString "fib"))
                                                )
                                                [ Located.located { end = { col = 11, row = 2 }, start = { col = 9, row = 2 } }
                                                    (Int 10)
                                                , Located.located { end = { col = 13, row = 2 }, start = { col = 12, row = 2 } }
                                                    (Int 0)
                                                , Located.located { end = { col = 15, row = 2 }, start = { col = 14, row = 2 } }
                                                    (Int 1)
                                                ]
                                            )
                                        )
                                    )
                                )
                            ]
                        )
        , Test.test "parse fails at +" <|
            \_ ->
                let
                    input : String
                    input =
                        [ "foo = \\a -> a + 1"
                        , "bar = \\x -> foo 1"
                        ]
                            |> String.join "\n"
                in
                parseString input
                    |> Expect.err
        ]



-- AST EXPRESSIONS


type alias LocatedExpr =
    Located Expr


type Expr
    = Int Int
    | Call LocatedExpr (List LocatedExpr)
    | Var Name
    | Lambda (List Name) LocatedExpr
    | Defs (List Def) LocatedExpr
    | If LocatedExpr LocatedExpr LocatedExpr



-- AST DEFINITIONS


type Def
    = Define (Located Name) LocatedExpr



-- AST MODULE


type alias Module =
    { imports : Dict ModuleName ()
    , values : List Value
    }


type Value
    = Value (Located Name) LocatedExpr
