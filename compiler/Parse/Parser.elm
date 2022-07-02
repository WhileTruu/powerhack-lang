module Parse.Parser exposing
    ( backslash
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
import Parse.Error as E
import Parser.Advanced as P exposing ((|.), (|=))


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


ignoreables : P.Parser context E.Problem ()
ignoreables =
    P.loop 0 <|
        ifProgress <|
            P.oneOf
                [ P.symbol (P.Token "\t" E.InvalidTab)
                    |> P.andThen (\_ -> P.problem E.InvalidTab)
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


backslash : P.Parser context E.Problem ()
backslash =
    P.token (P.Token "\\" E.ExpectingBackslash)


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


ignoreablesAndCheckIndent : (Int -> Int -> Bool) -> E.Problem -> P.Parser context E.Problem ()
ignoreablesAndCheckIndent check error =
    P.succeed ()
        |. ignoreables
        |. checkIndent check error


zeroOrMoreWith :
    P.Parser context problem ()
    -> P.Parser context problem a
    -> P.Parser context problem (List a)
zeroOrMoreWith spaces p =
    P.loop [] (zeroOrMoreHelp spaces p)


zeroOrMoreHelp :
    P.Parser context problem ()
    -> P.Parser context problem a
    -> List a
    -> P.Parser context problem (P.Step (List a) (List a))
zeroOrMoreHelp spaces p vs =
    P.oneOf
        [ p
            |> P.andThen
                (\v ->
                    let
                        result =
                            P.Loop (v :: vs)
                    in
                    P.oneOf
                        [ P.succeed result
                            |. spaces
                        , P.succeed result
                        ]
                )
        , P.succeed ()
            |> P.map (always (P.Done (List.reverse vs)))
        ]


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


notAtBeginningOfLine : P.Parser context E.Problem a -> P.Parser context E.Problem a
notAtBeginningOfLine parser =
    P.succeed identity
        |. checkIndent (\_ column -> column > 1) E.ExpectingIndentation
        |= parser


onlyAtBeginningOfLine : P.Parser context E.Problem a -> P.Parser context E.Problem a
onlyAtBeginningOfLine parser =
    P.succeed identity
        |. checkIndent (\_ column -> column == 1) E.ExpectingNoIndentation
        |= parser


rememberIndentation : P.Parser context problem a -> P.Parser context problem a
rememberIndentation parser =
    P.getCol
        |> P.andThen (\col -> P.withIndent col parser)
