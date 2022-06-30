module Parse.Parser exposing
    ( Parser
    , backslash
    , checkIndent
    , ignoreables
    , ignoreablesAndCheckIndent
    , located
    , notAtBeginningOfLine
    , oneOrMoreWith
    , onlyAtBeginningOfLine
    , rememberIndentation
    , spacesOnly
    , zeroOrMoreWith
    )

import Data.Located as Located exposing (Located)
import Parse.Error
import Parser.Advanced as PA exposing ((|.), (|=))


type alias Parser a =
    PA.Parser Parse.Error.Context Parse.Error.Problem a


located : Parser p -> Parser (Located p)
located p =
    PA.succeed
        (\( startRow, startCol ) value ( endRow, endCol ) ->
            Located.located
                { start = { row = startRow, col = startCol }
                , end = { row = endRow, col = endCol }
                }
                value
        )
        |= PA.getPosition
        |= p
        |= PA.getPosition


ignoreables : Parser ()
ignoreables =
    PA.loop 0 <|
        ifProgress <|
            PA.oneOf
                [ PA.symbol (PA.Token "\t" Parse.Error.InvalidTab)
                    |> PA.andThen (\_ -> PA.problem Parse.Error.InvalidTab)
                , PA.spaces
                ]


ifProgress : Parser a -> Int -> Parser (PA.Step Int ())
ifProgress parser offset =
    PA.succeed identity
        |. parser
        |= PA.getOffset
        |> PA.map
            (\newOffset ->
                if offset == newOffset then
                    PA.Done ()

                else
                    PA.Loop newOffset
            )


backslash : Parser ()
backslash =
    PA.token (PA.Token "\\" Parse.Error.ExpectingBackslash)


spacesOnly : Parser ()
spacesOnly =
    PA.chompWhile ((==) ' ')


oneOrMoreWith : Parser () -> Parser a -> Parser (List a)
oneOrMoreWith spaces p =
    PA.loop [] (oneOrMoreHelp spaces p)


oneOrMoreHelp : Parser () -> Parser a -> List a -> Parser (PA.Step (List a) (List a))
oneOrMoreHelp spaces p vs =
    PA.oneOf
        [ PA.succeed (\v -> PA.Loop (v :: vs))
            |= p
            |. spaces
        , PA.succeed ()
            |> PA.map (always (PA.Done (List.reverse vs)))
        ]


ignoreablesAndCheckIndent : (Int -> Int -> Bool) -> Parse.Error.Problem -> Parser ()
ignoreablesAndCheckIndent check error =
    PA.succeed ()
        |. ignoreables
        |. checkIndent check error


zeroOrMoreWith : Parser () -> Parser a -> Parser (List a)
zeroOrMoreWith spaces p =
    PA.loop [] (zeroOrMoreHelp spaces p)


zeroOrMoreHelp : Parser () -> Parser a -> List a -> Parser (PA.Step (List a) (List a))
zeroOrMoreHelp spaces p vs =
    PA.oneOf
        [ p
            |> PA.andThen
                (\v ->
                    let
                        result =
                            PA.Loop (v :: vs)
                    in
                    PA.oneOf
                        [ PA.succeed result
                            |. spaces
                        , PA.succeed result
                        ]
                )
        , PA.succeed ()
            |> PA.map (always (PA.Done (List.reverse vs)))
        ]


checkIndent : (Int -> Int -> Bool) -> Parse.Error.Problem -> Parser ()
checkIndent check error =
    PA.succeed
        (\indent col ->
            if check indent col then
                PA.succeed ()

            else
                PA.problem error
        )
        |= PA.getIndent
        |= PA.getCol
        |> PA.andThen identity


notAtBeginningOfLine : Parser a -> Parser a
notAtBeginningOfLine parser =
    PA.succeed identity
        |. checkIndent (\_ column -> column > 1) Parse.Error.ExpectingIndentation
        |= parser


onlyAtBeginningOfLine : Parser a -> Parser a
onlyAtBeginningOfLine parser =
    PA.succeed identity
        |. checkIndent (\_ column -> column == 1) Parse.Error.ExpectingNoIndentation
        |= parser


rememberIndentation : Parser a -> Parser a
rememberIndentation parser =
    PA.getCol
        |> PA.andThen (\col -> PA.withIndent col parser)
