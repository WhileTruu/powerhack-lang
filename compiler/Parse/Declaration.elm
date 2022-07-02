module Parse.Declaration exposing (Declaration(..), declaration)

import AST.Source as Source
import Parse.Error as E
import Parse.Expression
import Parse.Parser as P
import Parse.Variable
import Parser.Advanced as P exposing ((|.), (|=))


type Declaration
    = Value Source.Value


declaration : P.Parser E.Context E.Problem Declaration
declaration =
    P.succeed (\name body -> Value (Source.Value name body))
        |= P.onlyAtBeginningOfLine
            (Parse.Variable.variable
                |> P.located
            )
        |. P.ignoreables
        |= P.oneOf
            [ P.succeed identity
                |. P.notAtBeginningOfLine (P.symbol (P.Token ":" E.ExpectingEquals))
                |. P.ignoreables
                |= P.lazy (\_ -> Parse.Expression.expression)
            , P.succeed identity
                |. P.notAtBeginningOfLine (P.symbol (P.Token "=" E.ExpectingEquals))
                |. P.ignoreables
                |= P.lazy (\_ -> Parse.Expression.expression)
            ]
