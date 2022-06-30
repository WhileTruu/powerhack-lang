module Parse.Declaration exposing (Declaration(..), declaration)

import AST.Source as Source
import Parse.Error
import Parse.Expression
import Parse.Parser as Parser exposing (Parser)
import Parse.Variable
import Parser.Advanced as P exposing ((|.), (|=))


type Declaration
    = Value Source.Value


declaration : Parser Declaration
declaration =
    P.succeed (\name body -> Value (Source.Value name body))
        |= Parser.onlyAtBeginningOfLine
            (Parse.Variable.variable
                |> Parser.located
            )
        |. Parser.ignoreables
        |= P.oneOf
            [ P.succeed identity
                |. Parser.notAtBeginningOfLine (P.symbol (P.Token ":" Parse.Error.ExpectingEquals))
                |. Parser.ignoreables
                |= P.lazy (\_ -> Parse.Expression.expression)
            , P.succeed identity
                |. Parser.notAtBeginningOfLine (P.symbol (P.Token "=" Parse.Error.ExpectingEquals))
                |. Parser.ignoreables
                |= P.lazy (\_ -> Parse.Expression.expression)
            ]
