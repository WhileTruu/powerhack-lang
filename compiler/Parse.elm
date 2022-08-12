module Parse exposing
    ( Context(..)
    , Error(..)
    , Problem(..)
    , contextToString
    , expression
    , problemToString
    , run
    , testSuite
    )

import AST.Source as Source
import Data.FileContents as FileContents exposing (FileContents)
import Data.FilePath as FilePath exposing (FilePath)
import Data.Located as Located exposing (Located)
import Data.Name as Name exposing (Name)
import Expect
import Parser.Advanced as P exposing ((|.), (|=))
import Set
import String.Extra
import Test exposing (Test)


run : FilePath -> FileContents -> Result Error Source.Module
run filePath fileContents =
    P.run (module_ filePath) (FileContents.toString fileContents)
        |> Result.mapError (\a -> Error a fileContents filePath)



-- MODULE


module_ : FilePath -> P.Parser Context Problem Source.Module
module_ filePath =
    P.succeed (\( decl, decls ) -> categorizeDecls (decl :: decls))
        |= (oneOrMoreWith ignoreables declaration
                |> P.inContext (InFile filePath)
           )
        |. P.end ExpectingEnd


categorizeDecls : List Declaration -> { values : List Source.Value }
categorizeDecls decls =
    List.foldl
        (\decl { values } ->
            case decl of
                Value val ->
                    { values = val :: values }
        )
        { values = [] }
        decls



-- DECLARATION


type Declaration
    = Value Source.Value


declaration : P.Parser Context Problem Declaration
declaration =
    P.succeed (\name body -> Value (Source.Value name body))
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


term : P.Parser Context Problem Source.LocatedExpr
term =
    P.oneOf
        [ variable
            |> P.map Source.Var
            |> located
        , P.number
            { int = Result.Ok Source.Int
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


expression : P.Parser Context Problem Source.LocatedExpr
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


chompExprEnd : Source.LocatedExpr -> P.Parser Context Problem Source.LocatedExpr
chompExprEnd arg =
    P.loop [] (chompExprEndHelp arg)


chompExprEndHelp :
    Source.LocatedExpr
    -> List Source.LocatedExpr
    -> P.Parser Context Problem (P.Step (List Source.LocatedExpr) Source.LocatedExpr)
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
                P.succeed (Source.Call arg (List.reverse vs))
                    |> located
                    |> P.map P.Done
            ]


defsOrVarAndChompExprEnd : P.Parser Context Problem Source.LocatedExpr
defsOrVarAndChompExprEnd =
    P.loop [] defsOrVarAndChompExprEndHelp


defsOrVarAndChompExprEndHelp :
    List Source.Def
    -> P.Parser Context Problem (P.Step (List Source.Def) Source.LocatedExpr)
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
                        [ P.succeed (\body -> Source.Define name body)
                            |. P.symbol (P.Token "=" ExpectingEquals)
                            |. ignoreablesAndCheckIndent (<) ExpectingIndentation
                            |= P.lazy (\_ -> expression)
                            |> P.inContext InDef
                            |> P.map (\def2_ -> P.Loop (def2_ :: args))
                        , if List.isEmpty args then
                            varAndChompExprEnd name
                                |> P.map P.Done

                          else
                            P.succeed (\expr -> Source.Defs (List.reverse args) expr)
                                |= varAndChompExprEnd name
                                |> P.inContext InDefs
                                |> located
                                |> P.map P.Done
                        ]
                )
        , if List.isEmpty args then
            P.problem ExpectingDef

          else
            P.succeed (\expr -> Source.Defs (List.reverse args) expr)
                |= P.lazy (\_ -> expression)
                |> P.inContext InDefs
                |> located
                |> P.map P.Done
        ]


varAndChompExprEnd : Located Name -> P.Parser Context Problem Source.LocatedExpr
varAndChompExprEnd name =
    P.succeed (Source.Var (Located.toValue name))
        |> located
        |> P.andThen (\expr -> chompExprEnd expr)


lambda : P.Parser Context Problem Source.LocatedExpr
lambda =
    P.succeed (\( arg, args ) body -> Source.Lambda (arg :: args) body)
        |. backslash
        |= oneOrMoreWith spacesOnly variable
        |. spacesOnly
        |. P.symbol (P.Token "->" ExpectingRightArrow)
        |. ignoreablesAndCheckIndent (<) FuncIdentBody
        |= P.lazy (\_ -> expression)
        |> P.inContext InLambda
        |> located


if_ : P.Parser Context Problem Source.LocatedExpr
if_ =
    P.succeed Source.If
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
        , reserved = Set.fromList [ "if", "then", "else" ]
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
    = Error (List (P.DeadEnd Context Problem)) FileContents FilePath


type Context
    = InLambda
    | InDef
    | InDefs
    | InIf
    | InFile FilePath


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



-- TESTS


testSuite : Test
testSuite =
    let
        parseString : String -> Result Error (List Source.Value)
        parseString input =
            run (FilePath.init "Test.powerhack") (FileContents.init input)
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
                            [ Source.Value
                                (Located.located { end = { col = 4, row = 4 }, start = { col = 1, row = 4 } }
                                    (Name.fromString "fib")
                                )
                                (Located.located { end = { col = 34, row = 8 }, start = { col = 7, row = 4 } }
                                    (Source.Lambda [ Name.fromString "n", Name.fromString "a", Name.fromString "b" ]
                                        (Located.located { end = { col = 34, row = 8 }, start = { col = 5, row = 5 } }
                                            (Source.If
                                                (Located.located { end = { col = 15, row = 5 }, start = { col = 15, row = 5 } }
                                                    (Source.Call
                                                        (Located.located { end = { col = 11, row = 5 }, start = { col = 11, row = 5 } }
                                                            (Source.Var (Name.fromString "eq"))
                                                        )
                                                        [ Located.located { end = { col = 12, row = 5 }, start = { col = 11, row = 5 } }
                                                            (Source.Int 0)
                                                        , Located.located { end = { col = 14, row = 5 }, start = { col = 13, row = 5 } }
                                                            (Source.Var (Name.fromString "n"))
                                                        ]
                                                    )
                                                )
                                                (Located.located { end = { col = 10, row = 6 }, start = { col = 10, row = 6 } }
                                                    (Source.Var (Name.fromString "a"))
                                                )
                                                (Located.located { end = { col = 34, row = 8 }, start = { col = 34, row = 8 } }
                                                    (Source.Call
                                                        (Located.located { end = { col = 13, row = 8 }, start = { col = 13, row = 8 } }
                                                            (Source.Var (Name.fromString "fib"))
                                                        )
                                                        [ Located.located { end = { col = 21, row = 8 }, start = { col = 21, row = 8 } }
                                                            (Source.Call
                                                                (Located.located { end = { col = 18, row = 8 }, start = { col = 18, row = 8 } }
                                                                    (Source.Var (Name.fromString "sub"))
                                                                )
                                                                [ Located.located { end = { col = 19, row = 8 }, start = { col = 18, row = 8 } }
                                                                    (Source.Int 1)
                                                                , Located.located { end = { col = 21, row = 8 }, start = { col = 20, row = 8 } }
                                                                    (Source.Var (Name.fromString "n"))
                                                                ]
                                                            )
                                                        , Located.located { end = { col = 24, row = 8 }, start = { col = 23, row = 8 } }
                                                            (Source.Var (Name.fromString "b"))
                                                        , Located.located { end = { col = 33, row = 8 }, start = { col = 33, row = 8 } }
                                                            (Source.Call
                                                                (Located.located { end = { col = 30, row = 8 }, start = { col = 30, row = 8 } }
                                                                    (Source.Var (Name.fromString "add"))
                                                                )
                                                                [ Located.located { end = { col = 31, row = 8 }, start = { col = 30, row = 8 } }
                                                                    (Source.Var (Name.fromString "b"))
                                                                , Located.located { end = { col = 33, row = 8 }, start = { col = 32, row = 8 } }
                                                                    (Source.Var (Name.fromString "a"))
                                                                ]
                                                            )
                                                        ]
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            , Source.Value
                                (Located.located { end = { col = 5, row = 1 }, start = { col = 1, row = 1 } }
                                    (Name.fromString "main")
                                )
                                (Located.located { end = { col = 1, row = 4 }, start = { col = 8, row = 1 } }
                                    (Source.Lambda [ Name.fromString "arggg" ]
                                        (Located.located { end = { col = 1, row = 4 }, start = { col = 1, row = 4 } }
                                            (Source.Call
                                                (Located.located { end = { col = 9, row = 2 }, start = { col = 9, row = 2 } }
                                                    (Source.Var (Name.fromString "fib"))
                                                )
                                                [ Located.located { end = { col = 11, row = 2 }, start = { col = 9, row = 2 } }
                                                    (Source.Int 10)
                                                , Located.located { end = { col = 13, row = 2 }, start = { col = 12, row = 2 } }
                                                    (Source.Int 0)
                                                , Located.located { end = { col = 15, row = 2 }, start = { col = 14, row = 2 } }
                                                    (Source.Int 1)
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
