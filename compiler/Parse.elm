module Parse exposing (decls)

import Parse.Declaration as Declaration exposing (Declaration)
import Parse.Error as E
import Parse.Parser as P
import Parser.Advanced as P


decls : P.Parser E.Context E.Problem (List Declaration)
decls =
    P.oneOrMoreWith P.ignoreables Declaration.declaration
