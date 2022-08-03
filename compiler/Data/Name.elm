module Data.Name exposing (Name, fromString, toString)


type Name
    = Name String


fromString : String -> Name
fromString name =
    Name name


toString : Name -> String
toString (Name name) =
    name
