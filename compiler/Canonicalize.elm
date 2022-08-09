module Canonicalize exposing (canonicalize, canonicalizeExpr)

import AST.Canonical as Canonical
import AST.Source as Source
import Data.Located as Located
import Error exposing (Error)


canonicalize : Source.Module -> Result Error Canonical.Module
canonicalize source =
    List.foldl
        (\(Source.Value varName expr) acc ->
            Result.andThen
                (\values ->
                    canonicalizeExpr expr
                        |> Result.map
                            (\canonicalExpr ->
                                Canonical.Value varName canonicalExpr :: values
                            )
                )
                acc
        )
        (Result.Ok [])
        source.values
        |> Result.map (\a -> { values = a })
        |> Result.mapError Error.CanonicalizationError


canonicalizeExpr : Source.LocatedExpr -> Result Error.CanonicalizationError Canonical.LocatedExpr
canonicalizeExpr sourceExpr =
    case Located.toValue sourceExpr of
        Source.Int int ->
            Ok (Located.replaceWith (Canonical.Int int) sourceExpr)

        Source.Constructor name ->
            Ok (Located.replaceWith (Canonical.Constructor name (Debug.todo "")) sourceExpr)

        Source.Call fn arguments ->
            Result.andThen
                (\canonicalFn ->
                    List.foldl
                        (\arg acc ->
                            Result.andThen
                                (\fn_ ->
                                    canonicalizeExpr arg
                                        |> Result.map
                                            (\arg_ ->
                                                Located.replaceWith
                                                    (Canonical.Call fn_ arg_)
                                                    sourceExpr
                                            )
                                )
                                acc
                        )
                        (Result.Ok canonicalFn)
                        arguments
                )
                (canonicalizeExpr fn)

        Source.Var name ->
            Ok (Located.replaceWith (Canonical.Var name) sourceExpr)

        Source.Lambda arguments body ->
            Result.map
                (\canonicalBody ->
                    List.foldr
                        (\arg body_ ->
                            Located.replaceWith (Canonical.Lambda arg body_) sourceExpr
                        )
                        canonicalBody
                        arguments
                )
                (canonicalizeExpr body)

        Source.Defs defs expr ->
            Result.map2
                (\defs_ expr_ ->
                    Located.replaceWith (Canonical.Defs defs_ expr_) sourceExpr
                )
                (List.foldr
                    (\(Source.Define varName expr_) acc ->
                        Result.andThen
                            (\values ->
                                canonicalizeExpr expr_
                                    |> Result.map
                                        (\canonicalExpr ->
                                            Canonical.Define varName canonicalExpr :: values
                                        )
                            )
                            acc
                    )
                    (Result.Ok [])
                    defs
                )
                (canonicalizeExpr expr)

        Source.If test then_ else_ ->
            Result.map3
                (\test_ then__ else__ ->
                    Located.replaceWith (Canonical.If test_ then__ else__) sourceExpr
                )
                (canonicalizeExpr test)
                (canonicalizeExpr then_)
                (canonicalizeExpr else_)
