module Parse exposing (decls)

import Parse.Declaration as Declaration exposing (Declaration)
import Parse.Parser as Parser exposing (Parser)


decls : Parser (List Declaration)
decls =
    Parser.oneOrMoreWith Parser.ignoreables Declaration.declaration
