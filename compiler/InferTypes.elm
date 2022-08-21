module InferTypes exposing
    ( Annotation
    , Def(..)
    , Error
    , Expr
    , Expr_(..)
    , LocatedExpr
    , Module
    , SuperError
    , Type
    , TypeEnv(..)
    , Value(..)
    , errorToString
    , prettyScheme
    , prettySubst
    , prettyType
    , run
    )

import AST.Canonical as AST
import AssocList as Dict exposing (Dict)
import Console
import Data.Located as Located exposing (Located)
import Data.ModuleName exposing (ModuleName)
import Data.Name as Name exposing (Name)



-- RUN


primitives : Dict Name Type
primitives =
    -- FIXME These need to match the builtins in Emit modules
    Dict.fromList
        [ ( Name.fromString "add", TypeLambda typeInt (TypeLambda typeInt typeInt) )
        , ( Name.fromString "sub", TypeLambda typeInt (TypeLambda typeInt typeInt) )
        , ( Name.fromString "eq", TypeLambda typeInt (TypeLambda typeInt typeBool) )
        ]


type alias SuperError =
    { errors : List Error
    , subst : Subst
    , modules : Dict ModuleName Module
    }


run : Dict ModuleName AST.Module -> Result SuperError ( Dict ModuleName Module, Dict Name Annotation )
run canModules =
    let
        ( constraint, _, modulesWithFreshTypes ) =
            Dict.foldl
                (\k v ( cons, id, modulesWithFreshTypes1 ) ->
                    let
                        ( con, id1, moduleWithFreshTypes ) =
                            constrainModule id v
                    in
                    ( con :: cons, id1, Dict.insert k moduleWithFreshTypes modulesWithFreshTypes1 )
                )
                ( [], Id 0, Dict.empty )
                canModules
                |> (\( a, b, c ) -> ( CAnd a, b, c ))

        modulesEnv : Dict Name Type
        modulesEnv =
            modulesWithFreshTypes
                |> Dict.foldl
                    (\k v acc ->
                        v.values
                            |> List.map
                                (\(Value name expr) ->
                                    ( Located.toValue name, typeFromExpr expr )
                                )
                            |> Dict.fromList
                            |> Dict.union acc
                    )
                    Dict.empty

        { env, errors, subst } =
            solve (Dict.union modulesEnv primitives)
                { env = Dict.empty, subst = nullSubst, errors = [] }
                constraint

        typedModules : Dict ModuleName Module
        typedModules =
            modulesWithFreshTypes
                |> Dict.map
                    (\_ { values } ->
                        { values =
                            List.map
                                (\(Value name expr) ->
                                    Value name
                                        (recurse (Located.map (Tuple.mapSecond (applySubst subst)))
                                            expr
                                        )
                                )
                                values
                        }
                    )
    in
    case errors of
        [] ->
            Ok
                ( typedModules
                , Dict.map (\_ -> generalize (TypeEnv Dict.empty) << applySubst subst) env
                )

        e :: es ->
            Err
                { errors = List.map (applySubstInError subst) (e :: es)
                , subst = subst
                , modules = modulesWithFreshTypes
                }


applySubstInError : Subst -> Error -> Error
applySubstInError subst e =
    case e of
        UnificationFail region t1 t2 ->
            UnificationFail region (applySubst subst t1) (applySubst subst t2)

        InfiniteTypeFromOccurs region t1 t2 ->
            InfiniteTypeFromOccurs region (applySubst subst t1) (applySubst subst t2)

        InfiniteTypeFromBind region t1 t2 ->
            InfiniteTypeFromBind region (applySubst subst t1) (applySubst subst t2)

        UnboundVariable name ->
            UnboundVariable name



-- CONSTRAINTS


type Constraint
    = CEqual Located.Region Type Type
    | CAnd (List Constraint)
    | CLocal Located.Region Name Type
    | CLet
        { header : Dict (Located Name) Type
        , headerCon : Constraint
        , bodyCon : Constraint
        }
    | CTrue
    | CSaveTheEnvironment



-- TYPE


type Type
    = TypeVar Name
    | TypeLambda Type Type
    | TypeApplied Name.Name (List Type)


type Annotation
    = Forall FreeVars Type


type alias FreeVars =
    Dict Name ()



-- PRIMITIVE TYPES


typeInt : Type
typeInt =
    TypeApplied (Name.fromString "Int") []


typeBool : Type
typeBool =
    TypeApplied (Name.fromString "Bool") []



-- CONTEXT


type TypeEnv
    = TypeEnv (Dict Name Annotation)



-- NEW TYPE, ID


type Id
    = Id Int


fresh : Id -> ( Type, Id )
fresh (Id id) =
    ( TypeVar (Name.fromString ("u" ++ String.fromInt id)), Id (id + 1) )



-- GENERALIZATION


generalize : TypeEnv -> Type -> Annotation
generalize env t =
    let
        vars : FreeVars
        vars =
            Dict.diff (ftv t) (ftvEnv env)
    in
    Forall vars t


ftvEnv : TypeEnv -> FreeVars
ftvEnv (TypeEnv ctx) =
    List.foldl (\a acc -> Dict.union (ftvAnnotation a) acc) Dict.empty (Dict.values ctx)


ftvAnnotation : Annotation -> FreeVars
ftvAnnotation (Forall vars t) =
    Dict.diff (ftv t) vars


ftv : Type -> FreeVars
ftv ty =
    case ty of
        TypeVar var ->
            Dict.singleton var ()

        TypeLambda t1 t2 ->
            Dict.union (ftv t1) (ftv t2)

        TypeApplied _ _ ->
            -- TypeApplied name applied ->
            -- TODO: Find out what should go here, not sure this is correct
            -- there probably are ftvs in the applied types
            Dict.empty



-- CONSTRAIN


constrainModule : Id -> AST.Module -> ( Constraint, Id, Module )
constrainModule id { values } =
    constrainDecls id
        (List.map (\(AST.Value name expr) -> AST.Define name expr) values)
        CSaveTheEnvironment
        |> (\( con, id1, defs ) ->
                ( con
                , id1
                , { values = List.map (\(Define name expr) -> Value name expr) defs
                  }
                )
           )


constrainDecls : Id -> List AST.Def -> Constraint -> ( Constraint, Id, List Def )
constrainDecls id decls finalConstraint =
    case decls of
        def :: defs ->
            constrainRecursiveDefs id Dict.empty (def :: defs) finalConstraint

        [] ->
            ( finalConstraint, id, [] )


{-|


# Rigid type variables.


## From Google

_Rigid type variables (or skolem type variables, skolem constants, skolems).
They are fresh variables allocated to stand for unknown but fixed types.
Their actual types do not need to be, and cannot be determined._


## Elm compiler's comment about this dict

_As we step past type annotations, the free type variables are added to
the "rigid type variables" dict. Allowing sharing of rigid variables
between nested type annotations.
So if you have a top-level type annotation like (func : a -> b) the RTV
dictionary will hold variables for `a` and `b`_

-}
type alias RTV =
    Dict Name Type


constrain : Id -> RTV -> AST.LocatedExpr -> Type -> ( Constraint, Id, LocatedExpr )
constrain id rtv expr expected =
    let
        region : Located.Region
        region =
            Located.getRegion expr
    in
    case Located.toValue expr of
        AST.Var moduleName var ->
            ( CLocal region var expected
            , id
            , Located.located region ( Var moduleName var, expected )
            )

        AST.VarLocal var ->
            ( CLocal region var expected
            , id
            , Located.located region ( VarLocal var, expected )
            )

        AST.Lambda arg body ->
            constrainLambda id rtv region arg body expected

        AST.Call func arg ->
            constrainCall id rtv region func arg expected

        AST.Int value ->
            ( CEqual region typeInt expected
            , id
            , Located.located region ( Int value, typeInt )
            )

        AST.Defs defs body ->
            let
                ( bodyCon, id1, bodyExpr ) =
                    constrain id rtv body expected

                ( defsCon, id2, typedDefs ) =
                    constrainRecursiveDefs id1 rtv defs bodyCon
            in
            ( defsCon
            , id2
            , Located.located region ( Defs typedDefs bodyExpr, expected )
            )

        AST.If cond branch final ->
            constrainIf id rtv region cond branch final expected


constrainLambda : Id -> RTV -> Located.Region -> Name -> AST.LocatedExpr -> Type -> ( Constraint, Id, LocatedExpr )
constrainLambda id rtv region arg body expected =
    let
        ( argType, id1 ) =
            fresh id

        ( resultType, id2 ) =
            fresh id1

        ( bodyCon, id3, bodyExpr ) =
            constrain id2 rtv body resultType
    in
    ( CAnd
        [ CLet
            { header = Dict.singleton (Located.located region arg) argType
            , headerCon = CTrue
            , bodyCon = bodyCon
            }
        , CEqual region (TypeLambda argType resultType) expected
        ]
    , id3
    , Located.located region ( Lambda arg bodyExpr, expected )
    )


constrainCall : Id -> RTV -> Located.Region -> AST.LocatedExpr -> AST.LocatedExpr -> Type -> ( Constraint, Id, LocatedExpr )
constrainCall id rtv region func arg expected =
    let
        ( funcType, id1 ) =
            fresh id

        ( argType, id2 ) =
            fresh id1

        ( resultType, id3 ) =
            fresh id2

        ( funcCon, id4, funcExpr ) =
            constrain id3 rtv func funcType

        ( argCon, id5, argExpr ) =
            constrain id4 rtv arg argType

        funcRegion : Located.Region
        funcRegion =
            Located.getRegion func
    in
    ( CAnd
        [ funcCon
        , CEqual funcRegion funcType (TypeLambda argType resultType)
        , argCon
        , CEqual region resultType expected
        ]
    , id5
    , Located.located region ( Call funcExpr argExpr, expected )
    )


constrainIf : Id -> RTV -> Located.Region -> AST.LocatedExpr -> AST.LocatedExpr -> AST.LocatedExpr -> Type -> ( Constraint, Id, LocatedExpr )
constrainIf id rtv region cond branch final expected =
    let
        ( condCon, id1, condExpr ) =
            constrain id rtv cond typeBool

        ( branchType, id2 ) =
            fresh id1

        ( branchCon, id3, branchExpr ) =
            constrain id2 rtv branch branchType

        ( finalCon, id4, finalExpr ) =
            constrain id3 rtv final branchType
    in
    ( CAnd
        [ condCon
        , CAnd [ branchCon, finalCon ]
        , CEqual region branchType expected
        ]
    , id4
    , Located.located region ( If condExpr branchExpr finalExpr, expected )
    )


type alias Info =
    { cons : List Constraint
    , headers : Dict (Located Name) Type
    }


emptyInfo : Info
emptyInfo =
    { cons = [], headers = Dict.empty }


constrainRecursiveDefs : Id -> RTV -> List AST.Def -> Constraint -> ( Constraint, Id, List Def )
constrainRecursiveDefs id rtv defs bodyCon =
    recDefsHelp id rtv defs bodyCon emptyInfo []


recDefsHelp : Id -> RTV -> List AST.Def -> Constraint -> Info -> List Def -> ( Constraint, Id, List Def )
recDefsHelp id rtv defs bodyCon flexInfo typedDefs =
    case defs of
        [] ->
            ( CLet
                { header = flexInfo.headers
                , headerCon =
                    CLet
                        { header = flexInfo.headers
                        , headerCon = CTrue
                        , bodyCon = CAnd flexInfo.cons
                        }
                , bodyCon = bodyCon
                }
            , id
            , typedDefs
            )

        def :: otherDefs ->
            let
                (AST.Define name expr) =
                    def

                ( resultType, id1 ) =
                    fresh id

                ( exprCon, id2, typedExpr ) =
                    constrain id1 rtv expr resultType
            in
            (recDefsHelp id2 rtv otherDefs bodyCon <|
                { cons = exprCon :: flexInfo.cons
                , headers = Dict.insert name resultType flexInfo.headers
                }
            )
                (Define name typedExpr :: typedDefs)



-- SUBSTITUTION


type alias Subst =
    Dict Name Type


nullSubst : Subst
nullSubst =
    Dict.empty


composeSubst : Subst -> Subst -> Subst
composeSubst s1 s2 =
    Dict.union (Dict.map (\_ -> applySubst s1) s2) s1


applySubst : Subst -> Type -> Type
applySubst subst type_ =
    case type_ of
        TypeVar var ->
            Maybe.withDefault (TypeVar var) (Dict.get var subst)

        TypeLambda arg res ->
            TypeLambda (applySubst subst arg) (applySubst subst res)

        TypeApplied name applied ->
            -- TODO: Find out what should go here, not sure this is correct
            TypeApplied name (List.map (applySubst subst) applied)


prettySubst : Subst -> String
prettySubst subst =
    Dict.foldl
        (\name type_ acc ->
            (Name.toString name ++ " = " ++ Console.green (prettyType type_)) :: acc
        )
        []
        subst
        |> List.sortBy
            (\a ->
                String.dropLeft 1 a
                    |> String.split " "
                    |> List.head
                    |> Maybe.andThen String.toInt
                    |> Maybe.withDefault -1
            )
        |> String.join "\n"



-- SOLVE


type alias State =
    { env : Dict Name Type
    , subst : Subst
    , errors : List Error
    }


solve : RTV -> State -> Constraint -> State
solve rtv state constraint =
    case constraint of
        CEqual region t1 t2 ->
            let
                answer : Result ( UnifiesError, Subst ) Subst
                answer =
                    unifies (applySubst state.subst t1) (applySubst state.subst t2)
            in
            case answer of
                Ok subst ->
                    { state | subst = subst }

                Err ( err, subst ) ->
                    let
                        typeErr : Error
                        typeErr =
                            case err of
                                UnifiesErrorInfiniteTypeFromBind ty1 ty2 ->
                                    InfiniteTypeFromBind region ty1 ty2

                                UnifiesErrorUnificationFail ty1 ty2 ->
                                    UnificationFail region ty1 ty2
                    in
                    { state | errors = typeErr :: state.errors, subst = subst }

        CAnd constraints ->
            List.foldl
                (\constraint1 state1 ->
                    (\a -> { a | subst = composeSubst a.subst state1.subst })
                        (solve rtv state1 constraint1)
                )
                state
                constraints

        CLocal region name t ->
            case lookupRTV rtv (Located.located region name) of
                Ok actual ->
                    let
                        answer : Result ( UnifiesError, Subst ) Subst
                        answer =
                            unifies (applySubst state.subst actual) (applySubst state.subst t)
                    in
                    case answer of
                        Ok subst ->
                            { state | subst = subst }

                        Err ( err, subst ) ->
                            let
                                typeErr : Error
                                typeErr =
                                    case err of
                                        UnifiesErrorInfiniteTypeFromBind ty1 ty2 ->
                                            InfiniteTypeFromBind region ty1 ty2

                                        UnifiesErrorUnificationFail ty1 ty2 ->
                                            UnificationFail region ty1 ty2
                            in
                            { state | errors = typeErr :: state.errors, subst = subst }

                Err err ->
                    { state | errors = err :: state.errors }

        CLet { header, headerCon, bodyCon } ->
            -- Should RTV be env here, is it really still RTVs
            let
                headerWithoutRegions : Dict Name Type
                headerWithoutRegions =
                    Dict.foldl (\k v -> Dict.insert (Located.toValue k) v) Dict.empty header

                state1 : State
                state1 =
                    solve rtv state headerCon

                newEnv : Dict Name Type
                newEnv =
                    Dict.union rtv headerWithoutRegions

                state2 : State
                state2 =
                    solve newEnv state1 bodyCon
            in
            List.foldl occurs state2 (Dict.toList header)

        CTrue ->
            state

        CSaveTheEnvironment ->
            { state | env = rtv }


lookupRTV : RTV -> Located Name -> Result Error Type
lookupRTV rtv name =
    case Dict.get (Located.toValue name) rtv of
        Nothing ->
            Err (UnboundVariable name)

        Just type_ ->
            Ok type_


occurs : ( Located Name, Type ) -> State -> State
occurs ( name, type_ ) state =
    if occursCheck (Located.toValue name) type_ then
        { state | errors = InfiniteTypeFromOccurs (Located.getRegion name) (TypeVar (Located.toValue name)) type_ :: state.errors }

    else
        state


type UnifiesError
    = UnifiesErrorInfiniteTypeFromBind Type Type
    | UnifiesErrorUnificationFail Type Type


unifies : Type -> Type -> Result ( UnifiesError, Subst ) Subst
unifies t1 t2 =
    -- FIXME is there any benefit to adding the subst to the error here?
    -- That and the UnifiesError looks a bit annoying to handle in `solve`
    -- And also, should `unifies r r_` be checked if `unifies l l_` fails?
    if t1 == t2 then
        Ok nullSubst

    else
        case ( t1, t2 ) of
            ( TypeVar a, t ) ->
                bind a t
                    |> Result.mapError (\err -> ( err, nullSubst ))

            ( t, TypeVar a ) ->
                bind a t
                    |> Result.mapError (\err -> ( err, nullSubst ))

            ( TypeLambda l r, TypeLambda l_ r_ ) ->
                unifies l l_
                    |> Result.andThen
                        (\su1 ->
                            unifies r r_
                                |> Result.map (Dict.union su1)
                                |> Result.mapError (Tuple.mapSecond (Dict.union su1))
                        )

            _ ->
                Err ( UnifiesErrorUnificationFail t1 t2, nullSubst )


{-| Creates a fresh unification variable and binds it to the given type
-}
bind : Name -> Type -> Result UnifiesError Subst
bind a t =
    if t == TypeVar a then
        Ok Dict.empty

    else if occursCheck a t then
        Result.Err (UnifiesErrorInfiniteTypeFromBind (TypeVar a) t)

    else
        Ok (Dict.singleton a t)


occursCheck : Name -> Type -> Bool
occursCheck a t =
    Dict.member a (ftv t)



-- ERROR


type Error
    = UnificationFail Located.Region Type Type
    | InfiniteTypeFromOccurs Located.Region Type Type
    | InfiniteTypeFromBind Located.Region Type Type
    | UnboundVariable (Located Name)


errorToString : Error -> String
errorToString error =
    case error of
        UnificationFail _ _ _ ->
            "Unification fail."

        InfiniteTypeFromOccurs _ _ _ ->
            "Infinite type from occurs."

        InfiniteTypeFromBind _ _ _ ->
            "Infinite type from bind."

        UnboundVariable _ ->
            "Unbound variable"



-- PRETTY PRINTING


prettyScheme : Annotation -> String
prettyScheme (Forall vars ty) =
    case Dict.keys vars of
        [] ->
            prettyType ty

        _ ->
            let
                vars_ : List ( Name, String )
                vars_ =
                    List.indexedMap
                        (\i var -> ( var, generateVarName i ))
                        (List.sortBy (Name.toString >> String.dropLeft 1 >> String.toInt >> Maybe.withDefault 0)
                            (Dict.keys vars)
                        )

                renamedTy : Type
                renamedTy =
                    List.foldl renameTypeVar ty vars_
            in
            "∀ " ++ String.join " " (List.map Tuple.second vars_) ++ ". " ++ prettyType renamedTy


prettyType : Type -> String
prettyType ty =
    case ty of
        TypeVar var ->
            Name.toString var

        TypeApplied name types_ ->
            Name.toString name
                ++ (if not (List.isEmpty types_) then
                        " " ++ String.join " " (List.map prettyType types_)

                    else
                        ""
                   )

        TypeLambda ty1 ty2 ->
            (if isTypeLambda ty1 then
                "(" ++ prettyType ty1 ++ ")"

             else
                prettyType ty1
            )
                ++ " -> "
                ++ prettyType ty2


isTypeLambda : Type -> Bool
isTypeLambda ty =
    case ty of
        TypeLambda _ _ ->
            True

        _ ->
            False


renameTypeVar : ( Name, String ) -> Type -> Type
renameTypeVar ( old, new ) ty =
    case ty of
        TypeVar var ->
            TypeVar
                (if var == old then
                    Name.fromString new

                 else
                    var
                )

        TypeApplied name types_ ->
            TypeApplied
                (if name == old then
                    Name.fromString new

                 else
                    name
                )
                (List.map (renameTypeVar ( old, new )) types_)

        TypeLambda t1 t2 ->
            TypeLambda (renameTypeVar ( old, new ) t1) (renameTypeVar ( old, new ) t2)


generateVarName : Int -> String
generateVarName i =
    let
        char : Char
        char =
            Char.fromCode (97 + modBy 26 i)

        suffix : Int
        suffix =
            i // 26
    in
    String.fromChar char
        ++ (if suffix == 0 then
                ""

            else
                String.fromInt suffix
           )



-- AST EXPRESSIONS


type alias LocatedExpr =
    Located Expr


type alias Expr =
    ( Expr_, Type )


type Expr_
    = Int Int
    | Call LocatedExpr LocatedExpr
    | Var ModuleName Name
    | VarLocal Name
    | Lambda Name LocatedExpr
    | Defs (List Def) LocatedExpr
    | If LocatedExpr LocatedExpr LocatedExpr


recurse : (LocatedExpr -> LocatedExpr) -> LocatedExpr -> LocatedExpr
recurse fn locatedExpr =
    Located.map
        (\( expr, type_ ) ->
            case expr of
                Int _ ->
                    ( expr, type_ )

                Call func arg ->
                    ( Call (recurse fn func) (recurse fn arg), type_ )

                Var _ _ ->
                    ( expr, type_ )

                VarLocal _ ->
                    ( expr, type_ )

                Lambda name body ->
                    ( Lambda name (recurse fn body), type_ )

                Defs defs body ->
                    ( Defs
                        (List.map (\(Define name body_) -> Define name (recurse fn body_)) defs)
                        (recurse fn body)
                    , type_
                    )

                If cond branch final ->
                    ( If (recurse fn cond) (recurse fn branch) (recurse fn final)
                    , type_
                    )
        )
        (fn locatedExpr)


typeFromExpr : LocatedExpr -> Type
typeFromExpr expr =
    Tuple.second (Located.toValue expr)



-- AST DEFINITIONS


type Def
    = Define (Located Name) LocatedExpr



-- AST MODULE


type alias Module =
    { values : List Value
    }


type Value
    = Value (Located Name) LocatedExpr
