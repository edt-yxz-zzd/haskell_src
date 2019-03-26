
{-

pure type system

PTS:
    x
    k
    @x:A. B
        All
    ?x:A. B
        Some
    (A | B) # <@0:A, @1:B>
        Either

    \x:A. N
    (x=M. N : B)
    left M   # <@0=M>
    right N  # <@1=N>


    M N
    fst M # M .0
    snd N # N .1
    either R MorN
        :: (A -> R) -> (B -> R) -> R
        -- MkEitherCall

    M :: A
        assertion
    let x=M in N
    let (x, y)=M in N
    let {a=x, b=y, ...} = M in N
        let_expr

    -- named
    record = {x:A, y:B x, z:C x y}
        record .attr
        DAG_record <<== using var
        tuple_record <<== using @UInt
    choice = <x:A, y:B, z:C>
        var_choice <<== using var
        uint_choice <<== using @UInt


    case R M :: {x:A->R, y:B->R, z:C->R} -> R
    case R M N :: R
        M :: <x:A, y:B, z:C>
        N :: {x:A->R, y:B->R, z:C->R}

    -- concrete definition
    def @x,y. f(obj :: type, ...) = ... in ...
    def @x,y. f(case2 :: type2, ...) = ... in ...
    -- abstract definition
    def @x,y. f(obj :: type, ...) = ... : ... in ...
        -- abstract definition vs concrete definition:
        --  same: need concrete definition
        --  diff: donot care definition when compare
    -- virtual definition
    def @x,y. f(obj :: type, ...) : ... in ...
        -- virtual definition vs abstract/concrete definition
        --  diff: virtual definition has no concrete definition


    [=a=] :: [:A:]
        equivalence class


-}

module PTS where (
    Term, TypeTerm, ObjectTerm
    ,TermPattern, TypeTermPattern, ObjectTermPattern
    ,PTS
)

type TypeTerm = Term
type ObjectTerm = Term
--type KindTerm = Term
data Term var kind
    = MkVariable (Maybe var)    -- variable or "_"
    | MkKind kind               -- kind constant

    | MkAllType (Maybe var) (TypeTerm var kind) (TypeTerm var kind)
    | MkSomeType (Maybe var) (TypeTerm var kind) (TypeTerm var kind)
    | MkEitherType (TypeTerm var kind) (TypeTerm var kind)
    | MkRecordType [([var], var, (TypeTerm var kind))]
        -- using_vars :: [var]
        -- depedent on previous items
        -- form a DAG
    | MkChoiceType [(var, (TypeTerm var kind))]

    | MkAbstraction (Maybe var) (TypeTerm var kind) (ObjectTerm var kind)
    | MkPair (Maybe var) (ObjectTerm var kind) (ObjectTerm var kind) (Maybe (TypeTerm var kind))
    | MkLeft (ObjectTerm var kind)
    | MkRight (ObjectTerm var kind)
    | MkRecordObject [([var], var, (ObjectTerm var kind), Maybe (TypeTerm var kind))]
        -- if using_vars are empty, then (Maybe (TypeTerm var kind)) can be Nothing
    | MkChoiceObject var (ObjectTerm var kind)

    | MkApplication (ObjectTerm var kind) (ObjectTerm var kind)
    | MkFirst (ObjectTerm var kind)
    | MkSecond (ObjectTerm var kind)
    | MkEitherCall (TypeTerm var kind) (ObjectTerm var kind)
    | MkRecordAttribute (ObjectTerm var kind) var
    | MkCaseCall (TypeTerm var kind) (ObjectTerm var kind)

    | MkTupleAttribute (ObjectTerm var kind) UInt
    | MkEitherObjectAt (ObjectTerm var kind) UInt
    | MkAssertion (ObjectTerm var kind) (TypeTerm var kind)

    | MkLetExpression var (ObjectTerm var kind) (ObjectTerm var kind)
    | MkLetExpression_Tuple [Maybe var] (ObjectTerm var kind) (ObjectTerm var kind)
    | MkLetExpression_PartialRecord [(var, var)] (ObjectTerm var kind) (ObjectTerm var kind)
    | MkConcreteDefinition [var] var [TermPattern var kind] (ObjectTerm var kind) (ObjectTerm var kind)
    | MkAbstractDefinition [var] var [TermPattern var kind] (ObjectTerm var kind) (TypeTerm var kind) (ObjectTerm var kind)
    | MkVirtualDefinition [var] var [TermPattern var kind] (TypeTerm var kind) (ObjectTerm var kind)
    -- | MkMacroDefinition == MkConcreteDefinition??

    | MkDefinitionCall var [ObjectTerm var kind]
        -- f(a,b)
    -- | MkTermSchema (TermSchema var kind)
        -- f<PTS>(...)
        -- PTS => term

    | MkEquivalenceClassType (TypeTerm var kind)
    | MkEquivalenceClassObject (ObjectTerm var kind)

--data TermSchema var kind

type ObjectTermPattern = TermPattern
type TypeTermPattern = TermPattern
data TermPattern var kind
    = MkPatternAbstractVariable var
        -- ~=~ MkVariable
    | MkPatternExternalVariable var
        -- replace by external definition/declaration
    | MkPatternInternalVariable var
        -- MkPatternInternalVariable ==>> match pattern here
    | MkPatternKind kind
    | MkPatternAllType (Maybe var) (TypeTermPattern var kind) (TypeTermPattern var kind)
    | MkPatternSomeType (Maybe var) (TypeTermPattern var kind) (TypeTermPattern var kind)
    | MkPatternEitherType (TypeTermPattern var kind) (TypeTermPattern var kind)
    | MkPatternRecordType [([var], var, (TypeTermPattern var kind))]
    | MkPatternChoiceType [(var, (TypeTermPattern var kind))]

    | MkPatternAbstraction (Maybe var) (TypeTermPattern var kind) (ObjectTermPattern var kind)
    | MkPatternPair (Maybe var) (ObjectTermPattern var kind) (ObjectTermPattern var kind) (Maybe (TypeTermPattern var kind))
    | MkPatternLeft (ObjectTermPattern var kind)
    | MkPatternRight (ObjectTermPattern var kind)
    | MkPatternRecordObject [([var], var, (ObjectTermPattern var kind), Maybe (TypeTermPattern var kind))]
    | MkPatternChoiceObject var (ObjectTermPattern var kind)

    | MkPatternApplication (ObjectTermPattern var kind) (ObjectTermPattern var kind)
    | MkPatternFirst (ObjectTermPattern var kind)
    | MkPatternSecond (ObjectTermPattern var kind)
    | MkPatternEitherCall (TypeTermPattern var kind) (ObjectTermPattern var kind)
    | MkPatternRecordAttribute (ObjectTermPattern var kind) var
    | MkPatternCaseCall (TypeTermPattern var kind) (ObjectTermPattern var kind)

    | MkPatternAssertion (ObjectTermPattern var kind) (TypeTermPattern var kind)
    | MkPatternNonConcreteDefinitionCall var [ObjectTermPattern var kind]
    | MkPatternEquivalenceClassType (TypeTermPattern var kind)
    | MkPatternEquivalenceClassObject (ObjectTermPattern var kind)


class PTS pts where
    type KindOf pts :: *
    --type TermOf pts :: *
    type ObjectTermOf pts :: *
    type TypeTermOf pts :: *
    type ContextOf pts :: *
    is_kind :: pts -> KindOf pts -> Bool
        -- KindOf pts = Int; but only UInt is kind
    may_make_kind_of_All :: pts -> KindOf pts -> KindOf pts -> Maybe (KindOf pts)
    may_make_kind_of_Some :: pts -> KindOf pts -> KindOf pts -> Maybe (KindOf pts)
    may_make_kind_of_Record :: pts -> [KindOf pts] -> Maybe (KindOf pts)
    may_make_kind_of_Choice :: pts -> [KindOf pts] -> Maybe (KindOf pts)


    is_partial_type_le :: pts -> ContextOf pts -> TypeTermOf pts -> TypeTermOf pts -> Bool
    may_most_minimum_ancestor_of :: pts -> ContextOf pts -> TypeTermOf pts -> TypeTermOf pts -> Maybe (TypeTermOf pts)
    is_maybe_convergent :: pts -> Maybe Bool
    has_finite_kinds :: pts -> Bool






