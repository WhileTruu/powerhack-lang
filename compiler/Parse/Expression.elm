module Parse.Expression exposing (expression)

import AST.Source as Source
import Data.VarName exposing (VarName)
import Parse.Error as E
import Parse.Parser as P
import Parse.Variable
import Parser.Advanced as P exposing ((|.), (|=))


term : P.Parser E.Context E.Problem Source.LocatedExpr
term =
    P.oneOf
        [ Parse.Variable.variable
            |> P.map (\name -> Source.Var { name = name })
            |> P.located
        , P.number
            { int = Result.Ok Source.Int
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


expression : P.Parser E.Context E.Problem Source.LocatedExpr
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


chompExprEnd : Source.LocatedExpr -> P.Parser E.Context E.Problem Source.LocatedExpr
chompExprEnd arg =
    P.loop [] (chompExprEndHelp arg)


chompExprEndHelp :
    Source.LocatedExpr
    -> List Source.LocatedExpr
    -> P.Parser E.Context E.Problem (P.Step (List Source.LocatedExpr) Source.LocatedExpr)
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
                P.succeed
                    (Source.Call
                        { fn = arg
                        , arguments = List.reverse vs
                        }
                    )
                    |> P.located
                    |> P.map P.Done
            ]


defsOrVarAndChompExprEnd : P.Parser E.Context E.Problem Source.LocatedExpr
defsOrVarAndChompExprEnd =
    P.loop [] defsOrVarAndChompExprEndHelp


defsOrVarAndChompExprEndHelp :
    List Source.Def
    -> P.Parser E.Context E.Problem (P.Step (List Source.Def) Source.LocatedExpr)
defsOrVarAndChompExprEndHelp args =
    P.oneOf
        [ P.succeed identity
            |= Parse.Variable.variable
            |. P.spacesOnly
            |> P.andThen
                (\name ->
                    P.oneOf
                        [ P.succeed (\body -> Source.Define name body)
                            |. P.symbol (P.Token "=" E.ExpectingEquals)
                            |. P.ignoreablesAndCheckIndent (<) E.ExpectingIndentation
                            |= P.lazy (\_ -> expression)
                            |> P.inContext E.InDef
                            |> P.map (\def2_ -> P.Loop (def2_ :: args))
                        , if List.isEmpty args then
                            varAndChompExprEnd name
                                |> P.map P.Done

                          else
                            P.succeed (\expr -> Source.Defs (List.reverse args) expr)
                                |= varAndChompExprEnd name
                                |> P.inContext E.InDefs
                                |> P.located
                                |> P.map P.Done
                        ]
                )
        , if List.isEmpty args then
            P.problem E.ExpectingDef

          else
            P.succeed (\expr -> Source.Defs (List.reverse args) expr)
                |= P.lazy (\_ -> expression)
                |> P.inContext E.InDefs
                |> P.located
                |> P.map P.Done
        ]


varAndChompExprEnd : VarName -> P.Parser E.Context E.Problem Source.LocatedExpr
varAndChompExprEnd name =
    P.succeed (Source.Var { name = name })
        |> P.located
        |> P.andThen (\expr -> chompExprEnd expr)


lambda : P.Parser E.Context E.Problem Source.LocatedExpr
lambda =
    P.succeed
        (\arguments body ->
            Source.Lambda
                { arguments = arguments
                , body =
                    {- Run the promoting transformation on every subexpression,
                       so that after parsing all the arguments aren't unqualified
                       Vars but Arguments.

                       Ie. the lambda parser can't return:

                           -- \x -> x
                           Lambda { argument = VarName "x", body = Var (Nothing, VarName "x") }

                       And instead has to return:

                           -- \x -> x
                           Lambda { argument = VarName "x", body = Argument (VarName "x") }

                       TODO add a fuzz test for this invariant?
                    -}
                    body

                -- |> Located.map
                --     (Frontend.transform
                --         (promoteArguments arguments)
                --     )
                }
        )
        |. P.backslash
        |= P.oneOrMoreWith P.spacesOnly Parse.Variable.variable
        |. P.spacesOnly
        |. P.symbol (P.Token "->" E.ExpectingRightArrow)
        |. P.ignoreablesAndCheckIndent (<) E.FuncIdentBody
        |= P.lazy (\_ -> expression)
        |> P.inContext E.InLambda
        |> P.located


if_ : P.Parser E.Context E.Problem Source.LocatedExpr
if_ =
    P.succeed
        (\test then_ else_ ->
            Source.If
                { test = test
                , then_ = then_
                , else_ = else_
                }
        )
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
