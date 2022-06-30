module Data.VarName exposing (VarName, init, toString)


type VarName
    = VarName String


init : String -> VarName
init name =
    VarName name


toString : VarName -> String
toString (VarName name) =
    name
