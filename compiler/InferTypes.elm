module InferTypes exposing (Annotation, TypeError, errorToString, run, testSuite)

import AST.Canonical as AST
import AssocList as Dict exposing (Dict)
import Canonicalize
import Data.FileContents as FileContents
import Data.FilePath as FilePath
import Data.Located as Located
import Data.Name as Name exposing (Name)
import Expect
import Fuzz
import Parse
import Parse.Expression
import Parser.Advanced as P
import String.Extra
import Test exposing (Test)



-- RUN


primitives : RTV
primitives =
    -- FIXME These need to match the builtins in Compiler module
    Dict.fromList
        [ ( Name.fromString "add", TypeLambda typeInt (TypeLambda typeInt typeInt) )
        , ( Name.fromString "sub", TypeLambda typeInt (TypeLambda typeInt typeInt) )
        , ( Name.fromString "eq", TypeLambda typeInt (TypeLambda typeInt typeBool) )
        ]


run : AST.Module -> Result (List TypeError) (Dict Name Annotation)
run module_ =
    let
        { env, errors, subst } =
            solve primitives
                { env = Dict.empty, subst = nullSubst, errors = [] }
                (Tuple.first (constrainModule (Id 0) module_))
    in
    case errors of
        [] ->
            Ok (Dict.map (\_ -> generalize (TypeEnv Dict.empty) << applySubst subst) env)

        e :: es ->
            Err (e :: es)


runForExpr : AST.LocatedExpr -> Result (List TypeError) Annotation
runForExpr expr =
    let
        ( expectedType, id ) =
            fresh (Id 0)

        { env, errors, subst } =
            solve primitives
                { env = Dict.empty, subst = nullSubst, errors = [] }
                (Tuple.first (constrain id primitives expr expectedType))
    in
    case errors of
        [] ->
            Ok (generalize (TypeEnv Dict.empty) (applySubst subst expectedType))

        es ->
            Err es



-- CONSTRAINTS


type Constraint
    = CEqual Located.Region Type Type
    | CAnd (List Constraint)
    | CLocal Located.Region Name Type
    | CForeign Located.Region Name AST.Annotation Type
    | CLet
        { header : Dict Name Type
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

        TypeApplied name applied ->
            -- TODO: Find out what should go here, not sure this is correct
            Dict.empty



-- CONSTRAIN


constrainModule : Id -> AST.Module -> ( Constraint, Id )
constrainModule id { values } =
    constrainDecls id
        (List.map
            (\(AST.Value name expr) ->
                AST.Define (Located.unwrap name) expr
            )
            values
        )
        CSaveTheEnvironment


constrainDecls : Id -> List AST.Def -> Constraint -> ( Constraint, Id )
constrainDecls id decls finalConstraint =
    case decls of
        def :: defs ->
            let
                ( constraint, id1 ) =
                    -- FIXME? as there's only one kind of Def I think this could just be CSaveTheEnvironment directly
                    constrainDecls id defs finalConstraint
            in
            constrainRecursiveDefs id1 Dict.empty (def :: decls) constraint

        [] ->
            ( finalConstraint, id )


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
constrain id rtv expr expected =
    let
        region : Located.Region
        region =
            Located.getRegion expr
    in
    case Located.unwrap expr of
        AST.Var var ->
            ( CLocal region var expected, id )

        AST.Lambda arg body ->
            constrainLambda id rtv region arg body expected

        AST.Call func arg ->
            constrainCall id rtv region func arg expected

        AST.Int _ ->
            ( CEqual region typeInt expected, id )

        AST.Defs defs body ->
            let
                ( bodyCon, id1 ) =
                    constrain id rtv body expected
            in
            constrainRecursiveDefs id1 rtv defs bodyCon

        AST.If cond branch final ->
            constrainIf id rtv region cond branch final expected

        AST.Constructor name annotation ->
            ( CForeign region name annotation expected, id )


constrainLambda : Id -> RTV -> Located.Region -> Name -> AST.LocatedExpr -> Type -> ( Constraint, Id )
constrainLambda id rtv region arg body expected =
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
        , CEqual region (TypeLambda argType resultType) expected
        ]
    , id3
    )


constrainCall : Id -> RTV -> Located.Region -> AST.LocatedExpr -> AST.LocatedExpr -> Type -> ( Constraint, Id )
constrainCall id rtv region func arg expected =
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
    )


constrainIf : Id -> RTV -> Located.Region -> AST.LocatedExpr -> AST.LocatedExpr -> AST.LocatedExpr -> Type -> ( Constraint, Id )
constrainIf id rtv region cond branch final expected =
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
        , CEqual region branchType expected
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
    recDefsHelp id rtv defs bodyCon emptyInfo


recDefsHelp : Id -> RTV -> List AST.Def -> Constraint -> Info -> ( Constraint, Id )
recDefsHelp id rtv defs bodyCon flexInfo =
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
            )

        def :: otherDefs ->
            let
                (AST.Define name expr) =
                    def

                ( resultType, id1 ) =
                    fresh id

                ( exprCon, id2 ) =
                    constrain id1 rtv expr resultType
            in
            recDefsHelp id2 rtv otherDefs bodyCon <|
                { cons = exprCon :: flexInfo.cons
                , headers = Dict.insert name resultType flexInfo.headers
                }



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



-- SOLVE


type alias State =
    { env : Dict Name Type
    , subst : Subst
    , errors : List TypeError
    }


solve : RTV -> State -> Constraint -> State
solve rtv state constraint =
    case constraint of
        CEqual _ t1 t2 ->
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

        CLocal _ name t ->
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

        CForeign _ name (AST.Forall freeVars srcType) expectation ->
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
    {- FIXME is this correct-ish? Create a test?
       Iirc something like \a -> potato gave the annotation of `forall a. a`,
       is that related?
       Nope, not related and looks fine. :D

       Turns out the one I was thinking about was a recursive value `x = x + 1`.
       Those don't give errors and result in an `forall a. a`.
       https://gist.github.com/evancz/07436448b7d6c947f21742dab46d1218

       I suppose this has nothing to do with occurs at all.
    -}
    if occursCheck name type_ then
        { state | errors = InfiniteTypeFromOccurs (TypeVar name) type_ :: state.errors }

    else
        state


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
                unifies l l_
                    |> Result.andThen
                        (\su1 ->
                            unifies (applySubst su1 r) (applySubst su1 r_)
                                |> Result.map (Dict.union su1)
                        )

            ( _, _ ) ->
                Err (UnificationFail t1 t2)


{-| Creates a fresh unification variable and binds it to the given type
-}
bind : Name -> Type -> Result TypeError Subst
bind a t =
    if t == TypeVar a then
        Ok Dict.empty

    else if occursCheck a t then
        Result.Err (InfiniteTypeFromBind (TypeVar a) t)

    else
        Ok (Dict.singleton a t)


occursCheck : Name -> Type -> Bool
occursCheck a t =
    Dict.member a (ftv t)



-- ERROR


type TypeError
    = UnificationFail Type Type
    | InfiniteTypeFromOccurs Type Type
    | InfiniteTypeFromBind Type Type
    | UnboundVariable Name


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



-- TESTS


testSuite : Test
testSuite =
    let
        parseExprAndInferTypes : String -> Result String String
        parseExprAndInferTypes input =
            P.run Parse.Expression.expression input
                |> Result.mapError Debug.toString
                |> Result.andThen (Result.mapError Debug.toString << Canonicalize.canonicalizeExpr)
                |> Result.andThen (Result.mapError Debug.toString << runForExpr)
                |> Result.map prettyScheme

        parseAndInferType : String -> Result String String
        parseAndInferType input =
            Parse.parse (FilePath.init "Test.powerhack") (FileContents.init input)
                |> Result.mapError Debug.toString
                |> Result.andThen (Result.mapError Debug.toString << Canonicalize.canonicalize)
                |> Result.andThen (Result.mapError Debug.toString << run)
                |> Result.map
                    (Dict.foldl
                        (\k v a ->
                            a
                                ++ Name.toString k
                                ++ ": "
                                ++ prettyScheme v
                                ++ "\n"
                        )
                        ""
                    )
                |> Result.map (String.dropRight 1)

        primitiveTypes : List String
        primitiveTypes =
            [ "eq: Int -> Int -> Bool"
            , "sub: Int -> Int -> Int"
            , "add: Int -> Int -> Int"
            ]
    in
    Test.describe "Infer types"
        [ Test.test "variable" <|
            \_ ->
                "\\a -> a"
                    |> parseExprAndInferTypes
                    |> Expect.equal (Ok "∀ a. a -> a")
        , Test.test "variable 2" <|
            \_ ->
                """
                \\a ->
                    x = 1
                    1
                """
                    |> (String.Extra.unindent >> String.trim)
                    |> parseExprAndInferTypes
                    |> Expect.equal (Ok "∀ a. a -> Int")
        , Test.test "variable 3" <|
            \_ ->
                """
                \\x ->
                    fib = \\n a b ->
                        if eq 0 n then
                            a
                        else
                            fib (sub 1 n) b (add b a)
                    
                    fib 10 0 1
                """
                    |> (String.Extra.unindent >> String.trim)
                    |> parseExprAndInferTypes
                    |> Expect.equal (Ok "∀ a. a -> Int")
        , Test.test "parse & infer type" <|
            \_ ->
                let
                    expected : String
                    expected =
                        [ "fib: Int -> Int -> Int -> Int"
                        , "main: ∀ a. a -> Int"
                        ]
                            |> (++) primitiveTypes
                            |> String.join "\n"
                in
                """
                main = \\arggg ->
                    fib 10 0 1

                fib = \\n a b ->
                    if eq 0 n then
                        a
                    else
                        fib (sub 1 n) b (add b a)
                """
                    |> (String.Extra.unindent >> String.trim)
                    |> parseAndInferType
                    |> Expect.equal (Ok expected)
        , Test.test "parse & infer type basic" <|
            \_ ->
                let
                    input : String
                    input =
                        "foo = \\a -> 1"

                    expected : String
                    expected =
                        [ "foo: ∀ a. a -> Int"
                        ]
                            |> (++) primitiveTypes
                            |> String.join "\n"
                in
                parseAndInferType input
                    |> Expect.equal (Ok expected)
        , Test.test "unbound variable" <|
            \_ ->
                let
                    input : String
                    input =
                        [ "foo = \\a -> potato 1"
                        , ""
                        ]
                            |> String.join "\n"

                    expected : String
                    expected =
                        "[UnboundVariable (Name \"potato\"),UnboundVariable (Name \"potato\")]"
                in
                parseAndInferType input
                    |> Expect.equal (Err expected)
        , Test.skip <|
            Test.test "recursive value" <|
                -- FIXME some sort of cycle detection thing?
                -- https://gist.github.com/evancz/07436448b7d6c947f21742dab46d1218
                \_ ->
                    "x = x + 1"
                        |> parseAndInferType
                        |> Expect.err
        , Test.test "unification fail" <|
            \_ ->
                let
                    input : String
                    input =
                        [ "bar = \\x -> foo 1 1"
                        , "foo = \\a -> a + 1"
                        ]
                            |> String.join "\n"

                    expected : String
                    expected =
                        "[UnificationFail (TypeApplied (Name \"Int\") []) (TypeLambda (TypeVar (Name \"u10\")) (TypeVar (Name \"u11\"))),UnificationFail (TypeApplied (Name \"Int\") []) (TypeLambda (TypeVar (Name \"u19\")) (TypeVar (Name \"u20\")))]"
                in
                parseAndInferType input
                    |> Expect.equal (Err expected)
        , let
            fuzzer : Fuzz.Fuzzer String
            fuzzer =
                Fuzz.map2
                    (\a b ->
                        List.sortBy Tuple.first [ a, b ]
                            |> List.map Tuple.second
                            |> String.join "\n"
                    )
                    (Fuzz.map (\a -> ( a, "foo = \\a -> a + 1" )) Fuzz.int)
                    (Fuzz.map (\a -> ( a, "bar = \\x -> foo 1" )) Fuzz.int)
          in
          Test.fuzz fuzzer "definition order does not affect type" <|
            -- FIXME foo's type will be different based on the order of the definitions
            \input ->
                let
                    expected : String
                    expected =
                        [ "foo: Int -> Int"
                        , "bar: ∀ a. a -> Int"
                        ]
                            |> (++) primitiveTypes
                            |> String.join "\n"
                in
                parseAndInferType input
                    |> Expect.equal (Ok expected)
        , Test.todo "infinite type from occurs"
        , Test.todo "infinite type from bind"
        ]
