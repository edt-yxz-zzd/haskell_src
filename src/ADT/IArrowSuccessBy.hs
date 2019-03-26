{-# LANGUAGE DefaultSignatures #-}
-- {-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImplicitParams #-}

module ADT.IArrowSuccessBy
where

import Control.Arrow
import Seed.ArrowOps (switchA, constA, i2mo_to_i2iEo, eitherA)
import Seed.MaybeOps (isJust)
import Seed.EitherOps (mayRight)
import Seed.ProxyOps (withBy_, withBy, last1P)

import Control.Category
import Prelude hiding ((.), id)
import Seed.CategoryData (CategoryPair(..), CategoryTriple(..))
import ADT.IArrowCatch
import ADT.OpArrowPlusBy


class Arrow arr => IArrowPure arr where
    -- as-if no-side effect
    -- a *** b ^>> swap === swap ^>> b *** a
    -- fst ^>> a === a *** b >>^ fst
class IArrowThrowBy by arr => IArrowResetBy by arr where
    -- (ArrowZero | IArrowThrowBy by) is LeftZero
    -- here require ArrowZero is both left/right zero:
    -- i.e. cancel all the previous actions or reset to initial states
    --      a >>> zeroArrow >>> b === zeroArrow
    --      a *** zeroArrow *** b === zeroArrow
    --      a >>> (zeroArrow +++ b) ===
    --          if fail then as-if zeroArrow, i.e. no-op

class IArrowResetBy by arr => OpResetABy by arr where
    type ArrowWholeResetStateBy by arr
    tellWholeABy :: proxy by -> arr i (ArrowWholeResetStateBy by arr)
    resetWholeABy :: proxy by -> arr (ArrowWholeResetStateBy by arr) ()
    tellWholeABy_ :: (?by :: proxy by) => arr i (ArrowWholeResetStateBy by arr)
    resetWholeABy_ :: (?by :: proxy by) => arr (ArrowWholeResetStateBy by arr) ()
    tellWholeABy_ = withBy_ tellWholeABy
    resetWholeABy_ = withBy_ resetWholeABy
    tellWholeABy = withBy tellWholeABy_
    resetWholeABy = withBy resetWholeABy_
    {-# MINIMAL ( (tellWholeABy | tellWholeABy_)
                , (resetWholeABy | resetWholeABy)
                )
        #-}
class OpResetA arr where
    type ArrowWholeResetState arr
    tellWholeA :: arr i (ArrowWholeResetState arr)
    resetWholeA :: arr (ArrowWholeResetState arr) ()
class IArrowResetBy by arr => OpDoOrNopABy by arr where
    -- success or no side-effect
    do_or_nopABy :: proxy by -> arr i o -> arr i (Maybe o)
    do_or_nopEABy :: proxy by -> arr i o -> arr i (Either i o)
    do_or_nopABy_ :: (?by :: proxy by) => arr i o -> arr i (Maybe o)
    do_or_nopEABy_ :: (?by :: proxy by) => arr i o -> arr i (Either i o)
    -- {-# MINIMAL (do_or_nopABy | do_or_nopABy_) #-}
    do_or_nopEABy by = i2mo_to_i2iEo . do_or_nopABy by
    do_or_nopEABy_ = withBy_ do_or_nopEABy
    do_or_nopABy_ = withBy_ do_or_nopABy
    --do_or_nopABy = withBy do_or_nopABy_

    default do_or_nopABy
        :: (OpDetectSuccessABy by arr, ArrowChoice arr)
        => proxy by -> arr i o -> arr i (Maybe o)
    do_or_nopABy by a = switchA (detect_successABy by a) >>> right a >>^ mayRight
    --default do_or_nopA :: IArrowCatch arr => arr i o -> arr i (Maybe o)
    --do_or_nopA a = catchA_ex a >>^ mayRight
class IArrowResetBy by arr => OpDetectSuccessABy by arr where
    -- proof: OpDetectSuccessA+ArrowChoice is OpDoOrNopA
    --      see default implement of do_or_nopA
    -- zeroArrow as the fail state
    -- xxxx detect_successA :: arr i o -> arr i (Maybe o) xxxxx
    -- detect_successA is not like catchA
    --      it should not have side-effect;
    --      catchA handle after side-effect performs
    -- detectSA zeroArrow === constA False
    -- detectSA (arr f) === constA True
    -- detectSA (a *** b) === (detectSA a *** detectSA b) >>> andA
    -- detectSA (a +++ b) === (detectSA a +++ detectSA b) >>> id ||| id
    -- detectSA (a <+> b) === detectSA a &&& detectSA b >>> orA
    -- no side-effect
    detect_successABy :: proxy by -> arr i o -> arr i Bool
    detect_successABy_ :: (?by :: proxy by) => arr i o -> arr i Bool
    -- {-# MINIMAL (|) #-}
    detect_successABy_ = withBy_ detect_successABy
    default detect_successABy
        :: OpLookAheadABy by arr => proxy by -> arr i o -> arr i Bool
    detect_successABy by a = look_aheadABy by a >>> arr isJust

class OpDetectSuccessABy by arr => OpLookAheadABy by arr where
    -- proof: OpLookAheadA is OpDetectSuccessA
    --      see default implement of detect_successA
    look_aheadABy :: proxy by -> arr i o -> arr i (Maybe o) -- no side-effect
    look_aheadEABy :: proxy by -> arr i o -> arr i (Either i o)
    look_aheadABy_ :: (?by :: proxy by) => arr i o -> arr i (Maybe o) -- no side-effect
    look_aheadEABy_ :: (?by :: proxy by) => arr i o -> arr i (Either i o)
    -- {-# MINIMAL (|) #-}
    look_aheadEABy by = i2mo_to_i2iEo . look_aheadABy by
    look_aheadEABy_ = withBy_ look_aheadEABy
    look_aheadABy_ = withBy_ look_aheadABy
    {-
    default look_aheadA
        :: (IArrowPure arr, IArrowCatch arr) => proxy by -> arr i o -> arr i (Maybe o)
    look_aheadA a = catchA_ex a >>^ mayRight
    -- but a parser is not a pure arr!!
    -- it would have state and input...
    -}
    default look_aheadABy
        :: (OpResetABy by arr, IArrowCatchBy by arr)
        => proxy by -> arr i o -> arr i (Maybe o)
    look_aheadABy by i2o =
        (id &&& tellWholeABy by) >>> (i2mo *** resetWholeABy by) >>^ fst where
        i2mo = catchABy by i2o >>^ mayRight
class Arrow arr => OpSetSS arr where
    {-# MINIMAL (setSS) #-}
    set_ShouldSuccess, setSS, usingSS -- , (>>$$>>), (^^$$>>)
        :: arr i x -> arr x o -> arr i o
    set_ShouldSuccess = setSS
    -- (setSS a >>> b) - if a success then (a >>> b) 'should success'
    --      using in IArrowBiasedPlus, like ReadP's (<++)
    -- {-# MINIMAL (setSS, usingSS, detectSS) #-}
    default usingSS
        :: OpGetSS arr => arr i x -> arr x o -> arr i o
    usingSS lhs rhs = case getSS lhs of
        Chain2 h1 t1 -> setSS h1 $ t1 >>> rhs
    {-
    (>>$$>>) = setSS
    (^^$$>>) = usingSS
    -}
    {-
    set_ShouldSuccessABy, setSSBy, usingSSBy -- , (>>$$>>), (^^$$>>)
        :: proxy by -> arr i x -> arr x o -> arr i o
    set_ShouldSuccessABy_, setSSBy_, usingSSBy_
        :: (?by :: proxy by) => arr i x -> arr x o -> arr i o
    set_ShouldSuccessBy = setSSBy
    set_ShouldSuccessBy_ = setSSBy_
    usingSSBy_ = withBy_ usingSSBy
    setSSBy_ = withBy_ setSSBy
    -- (setSS a >>> b) - if a success then (a >>> b) 'should success'
    --      using in IArrowBiasedPlus, like ReadP's (<++)
    -- {-# MINIMAL (setSS, usingSS, detectSS) #-}
    {-# MINIMAL (setSSBy) #-}
    default usingSSBy
        :: OpGetSSBy by arr => proxy by -> arr i x -> arr x o -> arr i o
    usingSSBy by lhs rhs = case getSSBy by lhs of
        Chain2 h1 t1 -> setSSBy by h1 $ t1 >>> rhs
    -}
class (OpSetSS arr, OpDetectSuccessABy by arr)
    => IArrowSuccessBy by arr where
    -- proof IArrowSuccess is OpDetectSuccessA
    --      detect_successA a = detectSS $ setSS a id
    -- if 'should success' then kill input
    -- distinguish from ArrowError, which hold input to the end
    --
    -- assume special arrow (SS :: arr i i)
    --  A ::= any arrow
    --  M ::= arrow contains SS; marked
    --  U ::= arrow not contains SS = A - M; unmarked
    --  P ::= arr f; pure arrow
    --  R ::= impure arrow
    --  A = M | U
    --  U = (P | R) (>>> (P | R))* === P | R >>> P === R_ID >>> P
    --  R_ID = R | id
    --  removeSS arr ::= remove all SS in arr -- hence become U
    --  removeSS SS === id
    --  removeSS U === U
    --  removeSS (a `op` b) === removeSS a `op` removeSS b
    --      op === >>> | *** | +++ | <+>
    --  a >>> SS === removeSS a >>> SS    -- last SS remain
    --  P >>> SS === SS >>> P -- float up
    --  P >>> P === P'
    --  U >>> R === R'
    --  proof:
    --      for ArrowChoice/ArrowPlus:
    --          any arrow === R_ID >>> SS >>> U
    --          any U === R_ID >>> P
    --          OR: any arrow === R_ID >>> SS >>> R_ID >>> P
    --      1) basic case:
    --          1) SS === id >>> SS >>> id >>> id
    --          2) P === id >>> SS >>> id >>> P
    --          3) R === R >>> SS >>> id >>> id
    --          -- 4) zeroArrow/app is R
    --      2) a >>> b
    --          lhs@(R_ID >>> SS >>> U) >>> rhs@(R >>> SS >>> R_ID >>> P)
    --              === (removeSS lhs >>> R) >>> SS >>> R_ID >>> P
    --          lhs@(R_ID >>> SS >>> U) >>> rhs@(ID >>> SS >>> R_ID >>> P)
    --              === R_ID >>> SS >>> (U >>> removeSS rhs)
    --              === R_ID >>> SS >>> (U >>> R_ID) >>> P
    --          verify: id >>> a >>> id === a
    --              === R_ID >>> (P >>> P)
    --      3) first a
    --          first (R_ID >>> SS >>> U) === first R_ID >>> SS >>> first U
    --          first (R_ID >>> P) === first R_ID >>> first P
    --          first R === R
    --          first id === id
    --      3') a *** b
    --          a *** b ::= first a >>> swapA >>> first b >>> swapA
    --          !!ATOMIC!! (a >>> P) *** (b >>> P) === (a *** b) >>> (P *** P)
    --          lhs@(R_ID >>> SS >>> R_ID) *** rhs@(R_ID >>> SS >>> R_ID)
    --              === (removeSS lhs *** R_ID) >>> SS >>> (id *** R_ID)
    --          id *** id === id
    --          id *** R === R'
    --      4) left a
    --          left (R_ID >>> SS >>> U) === left R_ID >>> SS >>> left U
    --          left (R_ID >>> P) === left R_ID >>> left P
    --          left id === id
    --          left R === R'
    --      4') a +++ b
    --          -- !!ATOMIC!! (a+++b) >>> switchA === switchA >>> (b+++a)
    --          !!ATOMIC!! (a >>> c) +++ (b >>> d) === (a +++ b) >>> (c +++ d)
    --          lhs@(R_ID >>> SS >>> U) +++ rhs@(R_ID >>> SS >>> U)
    --              === (R_ID +++ R_ID) >>> SS >>> (U +++ U)
    --          (R_ID >>> P) +++ (R_ID >>> P) = (R_ID +++ R_ID) >>> (P +++ P)
    --          id +++ id === id
    --          R_ID +++ R_ID === R'
    --      5) a <+> b -- require ArrowChoice
    --          lhs@(id >>> SS >>> U) <+> rhs@(R_ID >>> SS >>> U)
    --          lhs@(R_ID >>> SS >>> U) <+> rhs@(id >>> SS >>> U)
    --              === id >>> SS >>> (removeSS lhs <+> removeSS rhs)
    --              === id >>> SS >>> (U' <+> U')
    --          U <+> U = (R_ID >>> P) <+> (R_ID >>> P)
    --              === (eitherA (+++) R_ID R_ID >>> (P ||| P))
    --
    --          lhs@(R >>> SS >>> U) <+> rhs@(R >>> SS >>> U)
    --              === (arr Left <+> arr Right) >>> (R +++ R)
    --                      >>> SS >>> (U ||| U)
    --              === eitherA (+++) R R >>> SS >>> (U ||| U)
    --          U ||| U = (R_ID >>> P) ||| (R_ID >>> P)
    --              === (R_ID +++ R_ID) >>> (P ||| P))
    --      6) do_or_nopA a -- require ArrowChoice
    --          do_or_nopA :: arr i o -> arr i (Maybe o)
    --          do_or_nopEA :: arr i o -> arr i (Either i o)
    --          do_or_nopA (do_or_nopA a) === do_or_nopA a >>^ Just
    --          do_or_nopA P === P >>^ Just  -- is a P
    --          do_or_nopA R === R'
    --          do_or_nopA (R_ID >>> SS >>> R_ID >>> P)
    --              === id >>> SS
    --                  -- R_ID
    --                  >>> do_or_nopA (R_ID >>> R_ID)
    --                  -- P
    --                  >>> fmapA P
    --          do_or_nopEA = i2mo_to_i2iEo . do_or_nopA
    --      7) a <<+> b -- require ArrowChoice
    --          a <<+> b ::= setSS a id <$+> b
    --          a <<+> b ::= do_or_nopEA a >>> (b ||| id)
    --          lhs <<+> rhs@(R_ID >>> SS >>> U)
    --              === (removeSS (do_or_nopEA lhs) >>> left R_ID)
    --                  >>> SS >>> (U ||| id)
    --          lhs@(R_ID1x >>> SS >>> R_ID1y >>> P1) <<+> rhs@(R_ID2 >>> SS >>> U2)
    --              === do_or_nopEA (R_ID1x >>> R_ID1y) >>> left R_ID2
    --                  >>> SS >>> (U2 ||| P1)
    --      8) a <$+> b -- require ArrowChoice
    --          lhs@(R_ID >>> SS >>> U) <$+> rhs
    --              ::= do_or_nopEA R_ID >>> (rhs ||| U)
    --          lhs@(R_ID1 >>> SS >>> U1) <$+> rhs@(R_ID2 >>> SS >>> U2)
    --              === (do_or_nopEA R_ID1 >>> left R_ID2) >>> SS >>> (U2 ||| U1)
    --      9) look_aheadA a -- require ArrowChoice
    --          look_aheadA :: arr i o -> arr i (Maybe o)
    --          although look_aheadA donot change state,
    --              it was affected by the underlying state
    --              impure!!
    --          look_aheadA (R_ID >>> SS >>> R_ID >>> P)
    --              === id >>> SS >>> look_aheadA (R_ID >>> R_ID) >>> fmapA P

    -- detectSS (U >>> SS >>> U) === detect_successA U
    --                          === setSS id $ detect_successA U
    -- usingSS (U >>> SS >>> U) rhs === setSS U (U >>> rhs)
    -- setSS lhs rhs === removeSS lhs >>> SS >>> removeSS rhs
    --
    -- more details:
    --      detectSS (id >>> SS >>> U) === arr (const True)
    --      detectSS (R >>> SS >>> U) === detect_successA R
    --      setSS lhs rhs === removeSS lhs >>> SS >>> removeSS rhs
    --          === (R_ID >>> P) >>> SS >>> (R_ID >>> P)
    --          === R_ID >>> SS >>> (P >>> R_ID >>> P)
    --          P >>> id >>> P === id >>> P'
    --          P >>> R >>> P  === R' >>> P

    -- no side-effect
    detect_ShouldSuccessBy, detectSSBy :: proxy by -> arr i o -> arr i Bool
    detect_ShouldSuccessBy_, detectSSBy_
        :: (?by :: proxy by) => arr i o -> arr i Bool
    {-
    (-|<) :: arr i o -> arr (Either i i) x -> arr i x
    a -|< b = switchA (detectSS a) >>> b
    -}
    detect_ShouldSuccessBy = detectSSBy
    detect_ShouldSuccessBy_ = detectSSBy_
    detectSSBy_ = withBy_ detectSSBy
    default detectSSBy
        :: OpGetSS arr => proxy by -> arr i o -> arr i Bool
    detectSSBy by a = case getSS a of
        Chain2 h t -> setSS id $ detect_successABy by h

{-
infixr 0 >>$$>> , ^^$$>> -- (_ >>> _) >>$$>> (_ >>> _ >>$$>> _)
infixr 1 -|< -- as >>>
default_detectSS :: IArrowBiasedPlus arr => arr i o -> arr i Bool
default_setSS :: IArrowBiasedPlus arr => arr i o -> arr i o
default_setSS = id
default_detectSS a = (a >>> constA True) <<+> constA False
-}


{-
class IArrowSuccessBy by arr => OpGetSSBy by arr where
    -- getSS (U >>> SS >>> U') ::= (U, U')
    get_ShouldSuccessBy, getSSBy
        :: proxy by -> arr i o -> CategoryPair arr i o
    get_ShouldSuccessBy_, getSSBy_
        :: (?by :: proxy by) => arr i o -> CategoryPair arr i o
    {-# MINIMAL (getSSBy | getSSBy_) #-}
    get_ShouldSuccessBy = getSSBy
    get_ShouldSuccessBy_ = getSSBy_
    getSSBy_ = withBy_ getSSBy
    getSSBy = withBy getSSBy_


class OpGetSSBy by arr => OpGetSSBy_Ex by arr where
    -- getSS_ex (R_ID >>> SS >>> R_ID' >>> P) ::= (R_ID, R_ID', P)
    get_ShouldSuccessBy_ex, getSSBy_ex
        :: proxy by -> arr i o -> CategoryTriple arr i o
    get_ShouldSuccessBy_ex_, getSSBy_ex_
        :: (?by :: proxy by) => arr i o -> CategoryTriple arr i o
    {-# MINIMAL (getSSBy_ex | getSSBy_ex_) #-}
    get_ShouldSuccessBy_ex = getSSBy_ex
    get_ShouldSuccessBy_ex_ = getSSBy_ex_
    getSSBy_ex_ = withBy_ getSSBy_ex
    getSSBy_ex = withBy getSSBy_ex_
-}
class OpSetSS arr => OpGetSS arr where
    -- getSS (U >>> SS >>> U') ::= (U, U')
    get_ShouldSuccess, getSS :: arr i o -> CategoryPair arr i o
    {-# MINIMAL (getSS) #-}
    get_ShouldSuccess = getSS


class OpGetSS arr => OpGetSS_Ex arr where
    -- getSS_ex (R_ID >>> SS >>> R_ID' >>> P) ::= (R_ID, R_ID', P)
    get_ShouldSuccess_ex, getSS_ex :: arr i o -> CategoryTriple arr i o
    {-# MINIMAL (getSS_ex) #-}
    get_ShouldSuccess_ex = getSS_ex

class IArrowSuccessBy by arr => IArrowBiasedPlusSSBy by arr where
    -- (U >>> SS >>> U') <$+> rhs ::=
    --      if U fail
    --      then reset state and call rhs
    --      else call U' -- rhs is drop, no matter U' fail or not
    -- lhs@(U >>> SS >>> U) <$+> rhs ::= do_or_nopEA U >>> (rhs ||| U)
    -- lhs@(U >>> SS >>> U) <$+> rhs ::=
    --      lhs <+> (((U >>^ Left) <<+> (rhs >>^ Right)) >>> (zeroArrow ||| id))
    -- no-effect before rhs when lhs fail; unlike catchA
    {-
    (<$+>), 
    (<$+>) = biased_plusSSBy
    -}
    biased_plusSSBy
        :: proxy by -> arr i o -> arr i o -> arr i o
    biased_plusSSBy_, (<$+>?)
        :: (?by :: proxy by) => arr i o -> arr i o -> arr i o
    (<$+>?) = biased_plusSSBy_
    biased_plusSSBy_ = withBy_ biased_plusSSBy
    default biased_plusSSBy
        :: (OpDoOrNopABy by arr, ArrowChoice arr, OpGetSS arr)
        => proxy by -> arr i o -> arr i o -> arr i o
    {- error:
    lhs <$+> rhs = case getSS lhs of
        Chain2 ss tail -> do_or_nopEA ss >>> (rhs ||| tail)
    -}
    biased_plusSSBy by lhs rhs = case (getSS lhs, getSS rhs) of
        (Chain2 h1 t1, Chain2 h2 t2) ->
            setSS (do_or_nopEABy by h1 >>> left h2) (t2 ||| t1)
class OpDoOrNopABy by arr => IArrowBiasedPlusBy by arr where
    -- proof: IArrowBiasedPlus is OpDoOrNopA
    --      do_or_nopA a = (a >>> arr Just) <<+> constA Nothing
    -- proof: OpDoOrNopA+ArrowChoice is IArrowBiasedPlus
    --      see default implement of <<+>
    -- no-effect before rhs when lhs fail; unlike catchA
    {-
    (<<+>), 
    (<<+>) = biased_plusBy
    -}
    biased_plusBy
        :: proxy by -> arr i o -> arr i o -> arr i o
    biased_plusBy_, (<<+>?)
        :: (?by :: proxy by) => arr i o -> arr i o -> arr i o
    (<<+>?) = biased_plusBy_
    biased_plusBy_ = withBy_ biased_plusBy
    --catch_beforeSS = (<<+>)
    {-
    default (<<+>)
        :: (ArrowPlus arr, IArrowSuccess arr, ArrowChoice arr)
        => arr i o -> arr i o -> arr i o
    f <<+> g = f <+> g' where
        --h = detect_ShouldSuccess f
        --g' = switchA h >>> (g ||| zeroArrow)
        g' = f -|< (g ||| zeroArrow)
    -}
    default biased_plusBy
        :: ArrowChoice arr => proxy by -> arr i o -> arr i o -> arr i o
    biased_plusBy by f g = do_or_nopEABy by f >>> (g ||| id)
--infixr 5 <<+> , <$+>

instance IArrowBiasedPlusBy by arr
    => OpArrowPlusBy (ByArrowBiasedPlus by) arr where
    --plusBy_ = (<<+>)
    plusBy = biased_plusBy . last1P
instance IArrowBiasedPlusSSBy by arr
    => OpArrowPlusBy (ByArrowBiasedPlusSS by) arr where
    --plusBy_ = (<$+>)
    plusBy = biased_plusSSBy . last1P

--}
--}
--}
--}
--}
