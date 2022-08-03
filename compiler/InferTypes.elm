module InferTypes exposing (Annotation, TypeError, errorToString, prettyScheme, run, run2)

import AST.Canonical as AST
import AssocList as Dict exposing (Dict)
import Data.Located as Located
import Data.Name as Name exposing (Name)


constrainModule : Id -> AST.Module -> ( Constraint, Id )
constrainModule id { values } =
    constrainDecls id values


constrainDecls : Id -> List AST.Value -> ( Constraint, Id )
constrainDecls id values =
    let
        decls =
            List.map
                (\(AST.Value name expr) ->
                    AST.Define (Located.unwrap name) expr
                )
                values
    in
    constrainRecursiveDefs id primitives decls CSaveTheEnvironment


run2 : AST.Module -> Result (List TypeError) (Dict Name Annotation)
run2 module_ =
    constrainModule (Id 0) module_
        |> Tuple.first
        |> solve primitives { env = Dict.empty, subst = nullSubst, errors = [] }
        |> (\state ->
                case state.errors |> Debug.log "errors" of
                    [] ->
                        Ok
                            (state.env
                                |> Dict.map
                                    (\_ a ->
                                        generalize (TypeEnv Dict.empty) (applySubst state.subst a)
                                    )
                            )

                    e :: es ->
                        Err (e :: es)
           )



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



-- RUN


run : AST.LocatedExpr -> Result TypeError Annotation
run expr =
    infer expr
        |> Result.map (\( s, t ) -> applySubst s t)
        |> Result.map (generalize (TypeEnv Dict.empty))


infer : AST.LocatedExpr -> Result TypeError ( Dict Name Type, Type )
infer expr =
    let
        ( expectedType, id ) =
            fresh (Id 0)
    in
    constrain id primitives expr expectedType
        |> Tuple.first
        |> solve primitives { env = Dict.empty, subst = nullSubst, errors = [] }
        |> (\state ->
                case state.errors |> Debug.log "errors" of
                    [] ->
                        Ok ( state.subst, expectedType )

                    e :: es ->
                        Err e
           )


primitives : RTV
primitives =
    Dict.fromList
        [ ( Name.fromString "identity"
          , TypeLambda (TypeVar (Name.fromString "a")) (TypeVar (Name.fromString "a"))
          )
        , ( Name.fromString "const"
          , TypeLambda
                (TypeVar (Name.fromString "a"))
                (TypeLambda
                    (TypeVar (Name.fromString "b"))
                    (TypeVar (Name.fromString "a"))
                )
          )
        , ( Name.fromString "add", TypeLambda typeInt (TypeLambda typeInt typeInt) )
        , ( Name.fromString "sub", TypeLambda typeInt (TypeLambda typeInt typeInt) )
        , ( Name.fromString "gte", TypeLambda typeInt (TypeLambda typeInt typeBool) )
        , ( Name.fromString "eq", TypeLambda typeInt (TypeLambda typeInt typeBool) )
        ]



-- CONTEXT


type TypeEnv
    = TypeEnv (Dict Name Annotation)



-- NEW TYPE, ID


type Id
    = Id Int


fresh : Id -> ( Type, Id )
fresh (Id id) =
    ( TypeVar (Name.fromString ("u" ++ String.fromInt id)), Id (id + 1) )



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

        TypeApplied name applied ->
            -- TODO: Find out what should go here, not sure this is correct
            Dict.empty



-- TYPING RULES


type Constraint
    = CEqual Type Type
    | CAnd (List Constraint)
    | CLocal Name Type
    | CForeign Name AST.Annotation Type
    | CLet
        { header : Dict Name Type
        , headerCon : Constraint
        , bodyCon : Constraint
        }
    | CTrue
    | CSaveTheEnvironment


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


constrain : Id -> RTV -> AST.LocatedExpr -> Type -> ( Constraint, Id )
constrain id rtv exp expected =
    case Located.unwrap exp of
        AST.Var var ->
            ( CLocal var expected, id )

        AST.Lambda arg body ->
            constrainLambda id rtv arg body expected

        AST.Call func arg ->
            constrainCall id rtv func arg expected

        AST.Int _ ->
            ( CEqual typeInt expected, id )

        AST.Defs defs body ->
            let
                ( bodyCon, id1 ) =
                    constrain id rtv body expected
            in
            constrainRecursiveDefs id1 rtv defs bodyCon

        AST.If cond branch final ->
            constrainIf id rtv cond branch final expected

        AST.Constructor name annotation ->
            ( CForeign name annotation expected, id )


constrainLambda : Id -> RTV -> Name -> AST.LocatedExpr -> Type -> ( Constraint, Id )
constrainLambda id rtv arg body expected =
    let
        ( argType, id1 ) =
            fresh id

        ( resultType, id2 ) =
            fresh id1

        ( bodyCon, id3 ) =
            constrain id2 rtv body resultType
    in
    ( CAnd
        [ CLet
            { header = Dict.singleton arg argType
            , headerCon = CTrue
            , bodyCon = bodyCon
            }
        , CEqual (TypeLambda argType resultType) expected
        ]
    , id3
    )


constrainCall : Id -> RTV -> AST.LocatedExpr -> AST.LocatedExpr -> Type -> ( Constraint, Id )
constrainCall id rtv func arg expected =
    let
        ( funcType, id1 ) =
            fresh id

        ( argType, id2 ) =
            fresh id1

        ( resultType, id3 ) =
            fresh id2

        ( funcCon, id4 ) =
            constrain id3 rtv func funcType

        ( argCon, id5 ) =
            constrain id4 rtv arg argType
    in
    ( CAnd
        [ funcCon
        , CEqual funcType (TypeLambda argType resultType)
        , argCon
        , CEqual resultType expected
        ]
    , id5
    )


constrainIf : Id -> RTV -> AST.LocatedExpr -> AST.LocatedExpr -> AST.LocatedExpr -> Type -> ( Constraint, Id )
constrainIf id rtv cond branch final expected =
    let
        ( condCon, id1 ) =
            constrain id rtv cond typeBool

        ( branchType, id2 ) =
            fresh id1

        ( branchCon, id3 ) =
            constrain id2 rtv branch branchType

        ( finalCon, id4 ) =
            constrain id3 rtv final branchType
    in
    ( CAnd
        [ condCon
        , CAnd [ branchCon, finalCon ]
        , CEqual branchType expected
        ]
    , id4
    )


type alias Info =
    { cons : List Constraint
    , headers : Dict Name Type
    }


emptyInfo : Info
emptyInfo =
    { cons = [], headers = Dict.empty }


constrainRecursiveDefs : Id -> RTV -> List AST.Def -> Constraint -> ( Constraint, Id )
constrainRecursiveDefs id rtv defs bodyCon =
    recDefsHelp id rtv defs bodyCon emptyInfo emptyInfo


recDefsHelp : Id -> RTV -> List AST.Def -> Constraint -> Info -> Info -> ( Constraint, Id )
recDefsHelp id rtv defs bodyCon rigidInfo flexInfo =
    -- TODO get rid of rigidInfo? I only have flex stuff.
    case defs of
        [] ->
            ( CLet
                { header = rigidInfo.headers
                , headerCon = CTrue
                , bodyCon =
                    CLet
                        { header = flexInfo.headers
                        , headerCon =
                            CLet
                                { header = flexInfo.headers
                                , headerCon = CTrue
                                , bodyCon = CAnd flexInfo.cons
                                }
                        , bodyCon = CAnd [ CAnd rigidInfo.cons, bodyCon ]
                        }
                }
            , id
            )

        def :: otherDefs ->
            let
                (AST.Define name expr) =
                    def

                ( argType, id1 ) =
                    fresh id

                ( resultType, id2 ) =
                    fresh id1

                ( exprCon, id3 ) =
                    constrain id2 rtv expr resultType

                defCon : Constraint
                defCon =
                    CLet
                        { header = Dict.empty
                        , headerCon = CEqual argType resultType
                        , bodyCon = exprCon
                        }
            in
            recDefsHelp id3 rtv otherDefs bodyCon rigidInfo <|
                { cons = defCon :: flexInfo.cons
                , headers = Dict.insert name argType flexInfo.headers
                }



-- CONSTRAINT SOLVER


unifies : Type -> Type -> Result TypeError Subst
unifies t1 t2 =
    if t1 == t2 then
        Ok nullSubst

    else
        case ( t1, t2 ) of
            ( TypeVar a, t ) ->
                bind a t

            ( t, TypeVar a ) ->
                bind a t

            ( TypeLambda l r, TypeLambda l_ r_ ) ->
                unifyMany [ l, r ] [ l_, r_ ]

            ( _, _ ) ->
                Err (UnificationFail t1 t2)


{-| Creates a fresh unification variable and binds it to the given type
-}
bind : Name -> Type -> Result TypeError Subst
bind a t =
    if t == TypeVar a then
        Ok Dict.empty

    else if occursCheck a t then
        Result.Err (InfiniteType (TypeVar a) t)

    else
        Ok (Dict.singleton a t)


occursCheck : Name -> Type -> Bool
occursCheck a t =
    Dict.member a (ftv t)


unifyMany : List Type -> List Type -> Result TypeError Subst
unifyMany xs1 xs2 =
    case ( xs1, xs2 ) of
        ( [], [] ) ->
            Ok nullSubst

        ( t1 :: ts1, t2 :: ts2 ) ->
            unifies t1 t2
                |> Result.andThen
                    (\su1 ->
                        unifyMany (List.map (applySubst su1) ts1) (List.map (applySubst su1) ts2)
                            |> Result.map (Dict.union su1)
                    )

        ( t1, t2 ) ->
            Err (UnificationMismatch t1 t2)


type alias State =
    { env : Dict Name Type
    , subst : Subst
    , errors : List TypeError
    }


solve : RTV -> State -> Constraint -> State
solve rtv state constraint =
    case constraint of
        CEqual t1 t2 ->
            let
                answer : Result TypeError Subst
                answer =
                    unifies (applySubst state.subst t1) (applySubst state.subst t2)
            in
            case answer of
                Ok subst ->
                    { state | subst = subst }

                Err err ->
                    { state | errors = err :: state.errors }

        CAnd constraints ->
            List.foldl
                (\constraint1 state1 ->
                    (\a -> { a | subst = composeSubst a.subst state1.subst })
                        (solve rtv state1 constraint1)
                )
                state
                constraints

        CLocal name t ->
            case lookupRTV rtv name of
                Ok actual ->
                    let
                        answer : Result TypeError Subst
                        answer =
                            unifies (applySubst state.subst actual) (applySubst state.subst t)
                    in
                    case answer of
                        Ok subst ->
                            { state | subst = subst }

                        Err err ->
                            { state | errors = err :: state.errors }

                Err err ->
                    { state | errors = err :: state.errors }

        CForeign name (AST.Forall freeVars srcType) expectation ->
            Debug.todo ""

        CLet { header, headerCon, bodyCon } ->
            -- Should RTV be env here, is it really still RTVs
            let
                state1 : State
                state1 =
                    solve rtv state headerCon

                newEnv : Dict Name Type
                newEnv =
                    Dict.union rtv header

                state2 : State
                state2 =
                    solve newEnv state1 bodyCon
            in
            List.foldl occurs state2 (Dict.toList header)

        CTrue ->
            state

        CSaveTheEnvironment ->
            { state | env = rtv }


lookupRTV : RTV -> Name -> Result TypeError Type
lookupRTV rtv x =
    case Dict.get x rtv of
        Nothing ->
            Err (UnboundVariable x)

        Just type_ ->
            Ok type_


occurs : ( Name, Type ) -> State -> State
occurs ( name, type_ ) state =
    -- FIXME is this correct-ish?
    if occursCheck name type_ then
        { state | errors = InfiniteType (TypeVar name) type_ :: state.errors }

    else
        state



-- ERROR


type TypeError
    = UnificationFail Type Type
    | InfiniteType Type Type
    | UnboundVariable Name
    | UnificationMismatch (List Type) (List Type)


errorToString : TypeError -> String
errorToString error =
    Debug.toString error



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
