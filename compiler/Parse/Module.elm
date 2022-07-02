module Parse.Module exposing (..)

import AST.Source as Source
import Data.FilePath exposing (FilePath)
import Parse.Declaration as Declaration exposing (Declaration)
import Parse.Error as E
import Parse.Parser as P
import Parser.Advanced as P exposing ((|=))


module_ : FilePath -> P.Parser E.Context E.Problem Source.Module
module_ filePath =
    P.succeed (\( decl, decls ) -> categorizeDecls (decl :: decls))
        |= P.oneOrMoreWith P.ignoreables Declaration.declaration
        |> P.inContext (E.InFile filePath)


categorizeDecls : List Declaration -> { values : List Source.Value }
categorizeDecls decls =
    List.foldl
        (\decl { values } ->
            case decl of
                Declaration.Value val ->
                    { values = val :: values }
        )
        { values = [] }
        decls
