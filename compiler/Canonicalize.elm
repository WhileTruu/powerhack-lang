module Canonicalize exposing (canonicalize, canonicalizeExpr)

import AST.Canonical as Canonical
import AST.Source as Source
import Data.Located as Located


canonicalize : Source.Module -> Canonical.Module
canonicalize source =
    let
        values : List Canonical.Value
        values =
            List.foldl
                (\(Source.Value varName expr) ->
                    (::) (Canonical.Value varName (canonicalizeExpr expr))
                )
                []
                source.values
    in
    { values = values }


canonicalizeExpr : Source.LocatedExpr -> Canonical.LocatedExpr
canonicalizeExpr sourceExpr =
    case Located.toValue sourceExpr of
        Source.Int int ->
            Located.replaceWith (Canonical.Int int) sourceExpr

        Source.Call fn arguments ->
            List.foldl
                (\arg acc ->
                    Located.replaceWith
                        (Canonical.Call acc (canonicalizeExpr arg))
                        sourceExpr
                )
                (canonicalizeExpr fn)
                arguments

        Source.Var name ->
            Located.replaceWith (Canonical.Var name) sourceExpr

        Source.Lambda arguments body ->
            List.foldr
                (\arg body_ ->
                    Located.replaceWith (Canonical.Lambda arg body_) sourceExpr
                )
                (canonicalizeExpr body)
                arguments

        Source.Defs defs body ->
            let
                canDefs : List Canonical.Def
                canDefs =
                    List.foldr
                        (\(Source.Define varName expr_) acc ->
                            Canonical.Define varName (canonicalizeExpr expr_) :: acc
                        )
                        []
                        defs

                canBody : Canonical.LocatedExpr
                canBody =
                    canonicalizeExpr body
            in
            Located.replaceWith (Canonical.Defs canDefs canBody) sourceExpr

        Source.If test then_ else_ ->
            Located.replaceWith
                (Canonical.If
                    (canonicalizeExpr test)
                    (canonicalizeExpr then_)
                    (canonicalizeExpr else_)
                )
                sourceExpr
