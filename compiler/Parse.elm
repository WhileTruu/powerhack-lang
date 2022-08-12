module Parse exposing (run, testSuite)

import AST.Source as Source
import Data.FileContents as FileContents exposing (FileContents)
import Data.FilePath as FilePath exposing (FilePath)
import Data.Located as Located
import Data.Name as Name
import Error exposing (Error)
import Expect
import Parse.Module
import Parser.Advanced as P
import String.Extra
import Test exposing (Test)


run : FilePath -> FileContents -> Result Error Source.Module
run filePath fileContents =
    P.run (Parse.Module.module_ filePath) (FileContents.toString fileContents)
        |> Result.mapError (\a -> Error.ParseError a fileContents filePath)


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
