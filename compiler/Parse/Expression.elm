module Parse.Expression exposing (expression)

import AST.Source as AST
import Data.Located as Located exposing (Located)
import Data.Name exposing (Name)
import Parse.Error as E
import Parse.Parser as P
import Parse.Variable
import Parser.Advanced as P exposing ((|.), (|=))


term : P.Parser E.Context E.Problem AST.LocatedExpr
term =
    P.oneOf
        [ Parse.Variable.variable
            |> P.map AST.Var
            |> P.located
        , P.number
            { int = Result.Ok AST.Int
            , hex = Result.Err E.InvalidNumber
            , octal = Result.Err E.InvalidNumber
            , binary = Result.Err E.InvalidNumber
            , float = Result.Err E.InvalidNumber
            , invalid = E.InvalidNumber
            , expecting = E.ExpectingNumber
            }
            |> P.located
            -- FIXME remove backtrackable
            |> P.backtrackable
        , P.succeed identity
            |. P.symbol (P.Token "(" E.ExpectingOpenParen)
            |= P.lazy (\_ -> expression)
            |. P.symbol (P.Token ")" E.ExpectingCloseParen)
        ]


expression : P.Parser E.Context E.Problem AST.LocatedExpr
expression =
    P.oneOf
        [ if_
        , lambda
        , P.rememberIndentation defsOrVarAndChompExprEnd
        , term
            |> P.andThen
                (\expr ->
                    chompExprEnd expr
                )
        ]


chompExprEnd : AST.LocatedExpr -> P.Parser E.Context E.Problem AST.LocatedExpr
chompExprEnd arg =
    P.loop [] (chompExprEndHelp arg)


chompExprEndHelp :
    AST.LocatedExpr
    -> List AST.LocatedExpr
    -> P.Parser E.Context E.Problem (P.Step (List AST.LocatedExpr) AST.LocatedExpr)
chompExprEndHelp arg vs =
    P.succeed identity
        |. P.ignoreables
        |= P.oneOf
            [ P.succeed identity
                |. P.checkIndent (<) E.ExpectingIndentation
                |= term
                |> P.map (\v -> P.Loop (v :: vs))
            , if List.isEmpty vs then
                P.succeed (P.Done arg)

              else
                P.succeed (AST.Call arg (List.reverse vs))
                    |> P.located
                    |> P.map P.Done
            ]


defsOrVarAndChompExprEnd : P.Parser E.Context E.Problem AST.LocatedExpr
defsOrVarAndChompExprEnd =
    P.loop [] defsOrVarAndChompExprEndHelp


defsOrVarAndChompExprEndHelp :
    List AST.Def
    -> P.Parser E.Context E.Problem (P.Step (List AST.Def) AST.LocatedExpr)
defsOrVarAndChompExprEndHelp args =
    P.oneOf
        [ P.succeed identity
            |= (Parse.Variable.variable
                    |> P.located
               )
            |. P.spacesOnly
            |> P.andThen
                (\name ->
                    P.oneOf
                        [ P.succeed (\body -> AST.Define name body)
                            |. P.symbol (P.Token "=" E.ExpectingEquals)
                            |. P.ignoreablesAndCheckIndent (<) E.ExpectingIndentation
                            |= P.lazy (\_ -> expression)
                            |> P.inContext E.InDef
                            |> P.map (\def2_ -> P.Loop (def2_ :: args))
                        , if List.isEmpty args then
                            varAndChompExprEnd name
                                |> P.map P.Done

                          else
                            P.succeed (\expr -> AST.Defs (List.reverse args) expr)
                                |= varAndChompExprEnd name
                                |> P.inContext E.InDefs
                                |> P.located
                                |> P.map P.Done
                        ]
                )
        , if List.isEmpty args then
            P.problem E.ExpectingDef

          else
            P.succeed (\expr -> AST.Defs (List.reverse args) expr)
                |= P.lazy (\_ -> expression)
                |> P.inContext E.InDefs
                |> P.located
                |> P.map P.Done
        ]


varAndChompExprEnd : Located Name -> P.Parser E.Context E.Problem AST.LocatedExpr
varAndChompExprEnd name =
    P.succeed (AST.Var (Located.unwrap name))
        |> P.located
        |> P.andThen (\expr -> chompExprEnd expr)


lambda : P.Parser E.Context E.Problem AST.LocatedExpr
lambda =
    P.succeed (\( arg, args ) body -> AST.Lambda (arg :: args) body)
        |. P.backslash
        |= P.oneOrMoreWith P.spacesOnly Parse.Variable.variable
        |. P.spacesOnly
        |. P.symbol (P.Token "->" E.ExpectingRightArrow)
        |. P.ignoreablesAndCheckIndent (<) E.FuncIdentBody
        |= P.lazy (\_ -> expression)
        |> P.inContext E.InLambda
        |> P.located


if_ : P.Parser E.Context E.Problem AST.LocatedExpr
if_ =
    P.succeed AST.If
        |. P.keyword (P.Token "if" E.ExpectingIf)
        |. P.ignoreables
        |= P.lazy (\_ -> expression)
        |. P.ignoreables
        |. P.keyword (P.Token "then" E.ExpectingThen)
        |. P.ignoreables
        |= P.lazy (\_ -> expression)
        |. P.ignoreables
        |. P.keyword (P.Token "else" E.ExpectingElse)
        |. P.ignoreables
        |= P.lazy (\_ -> expression)
        |> P.inContext E.InIf
        |> P.located
