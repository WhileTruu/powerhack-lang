module Data.ModuleName exposing (ModuleName, fromString, toString)


type ModuleName
    = ModuleName String


fromString : String -> ModuleName
fromString name =
    ModuleName name


toString : ModuleName -> String
toString (ModuleName name) =
    name
