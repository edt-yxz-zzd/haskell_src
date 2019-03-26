{-# LANGUAGE DefaultSignatures #-}
-- {-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module ADT.IArrowSuccess
where

import Control.Arrow
import Seed.ArrowOps (switchA, constA, i2mo_to_i2iEo, eitherA)
import Seed.MaybeOps (isJust)
import Seed.EitherOps (mayRight)
import Control.Category
import Prelude hiding ((.), id)
import Seed.CategoryData (CategoryPair(..), CategoryTriple(..))
import ADT.IArrowCatch
import ADT.OpArrowPlusBy


class Arrow arr => IArrowPure arr where
    -- as-if no-side effect
    -- a *** b ^>> swap === swap ^>> b *** a
    -- fst ^>> a === a *** b >>^ fst
class ArrowZero arr => IArrowReset arr where
    -- ArrowZero is LeftZero
    -- here require ArrowZero is both left/right zero:
    -- i.e. cancel all the previous actions or reset to initial states
    --      a >>> zeroArrow >>> b === zeroArrow
    --      a *** zeroArrow *** b === zeroArrow
    --      a >>> (zeroArrow +++ b) ===
    --          if fail then as-if zeroArrow, i.e. no-op

class IArrowReset arr => OpResetA arr where
    type ArrowWholeResetState arr
    tellWholeA :: arr i (ArrowWholeResetState arr, i)
    resetWholeA :: arr (ArrowWholeResetState arr, o) o
class IArrowReset arr => OpDoOrNopA arr where
    -- success or no side-effect
    do_or_nopA :: arr i o -> arr i (Maybe o)
    do_or_nopEA :: arr i o -> arr i (Either i o)
    do_or_nopEA = i2mo_to_i2iEo . do_or_nopA

    default do_or_nopA
        :: (OpDetectSuccessA arr, ArrowChoice arr)
        => arr i o -> arr i (Maybe o)
    do_or_nopA a = switchA (detect_successA a) >>> right a >>^ mayRight
    --default do_or_nopA :: IArrowCatch arr => arr i o -> arr i (Maybe o)
    --do_or_nopA a = catchA_ex a >>^ mayRight
class IArrowReset arr => OpDetectSuccessA arr where
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
    detect_successA :: arr i o -> arr i Bool
    default detect_successA :: OpLookAheadA arr => arr i o -> arr i Bool
    detect_successA a = look_aheadA a >>> arr isJust

class OpDetectSuccessA arr => OpLookAheadA arr where
    -- proof: OpLookAheadA is OpDetectSuccessA
    --      see default implement of detect_successA
    look_aheadA :: arr i o -> arr i (Maybe o) -- no side-effect
    look_aheadEA :: arr i o -> arr i (Either i o)
    look_aheadEA = i2mo_to_i2iEo . look_aheadA
    {-
    default look_aheadA
        :: (IArrowPure arr, IArrowCatch arr) => arr i o -> arr i (Maybe o)
    look_aheadA a = catchA_ex a >>^ mayRight
    -- but a parser is not a pure arr!!
    -- it would have state and input...
    -}
    default look_aheadA
        :: (OpResetA arr, IArrowCatch arr) => arr i o -> arr i (Maybe o)
    look_aheadA i2o = tellWholeA >>> second i2mo >>> resetWholeA where
        i2mo = catchA_ex i2o >>^ mayRight
class OpDetectSuccessA arr => IArrowSuccess arr where
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
    detect_ShouldSuccess, detectSS :: arr i o -> arr i Bool
    (-|<) :: arr i o -> arr (Either i i) x -> arr i x
    set_ShouldSuccess, setSS, usingSS, (>>$$>>), (^^$$>>)
        :: arr i x -> arr x o -> arr i o
    a -|< b = switchA (detectSS a) >>> b
    (>>$$>>) = setSS
    (^^$$>>) = usingSS
    set_ShouldSuccess = setSS
    detect_ShouldSuccess = detectSS
    -- (setSS a >>> b) - if a success then (a >>> b) 'should success'
    --      using in IArrowBiasedPlus, like ReadP's (<++)
    -- {-# MINIMAL (setSS, usingSS, detectSS) #-}
    {-# MINIMAL (setSS) #-}
    default usingSS
        :: OpGetSS arr => arr i x -> arr x o -> arr i o
    usingSS lhs rhs = case getSS lhs of
        Chain2 h1 t1 -> setSS h1 $ t1 >>> rhs
    default detectSS :: OpGetSS arr => arr i o -> arr i Bool
    detectSS a = case getSS a of
        Chain2 h t -> setSS id $ detect_successA h

infixr 0 >>$$>> , ^^$$>> -- (_ >>> _) >>$$>> (_ >>> _ >>$$>> _)
infixr 1 -|< -- as >>>
default_detectSS :: IArrowBiasedPlus arr => arr i o -> arr i Bool
default_setSS :: IArrowBiasedPlus arr => arr i o -> arr i o
default_setSS = id
default_detectSS a = (a >>> constA True) <<+> constA False


class IArrowSuccess arr => OpGetSS arr where
    -- getSS (U >>> SS >>> U') ::= (U, U')
    get_ShouldSuccess, getSS :: arr i o -> CategoryPair arr i o
    get_ShouldSuccess = getSS

class OpGetSS arr => OpGetSS_Ex arr where
    -- getSS_ex (R_ID >>> SS >>> R_ID' >>> P) ::= (R_ID, R_ID', P)
    get_ShouldSuccess_ex, getSS_ex :: arr i o -> CategoryTriple arr i o
    get_ShouldSuccess_ex = getSS_ex
class IArrowSuccess arr => IArrowBiasedPlusSS arr where
    -- (U >>> SS >>> U') <$+> rhs ::=
    --      if U fail
    --      then reset state and call rhs
    --      else call U' -- rhs is drop, no matter U' fail or not
    -- lhs@(U >>> SS >>> U) <$+> rhs ::= do_or_nopEA U >>> (rhs ||| U)
    -- lhs@(U >>> SS >>> U) <$+> rhs ::=
    --      lhs <+> (((U >>^ Left) <<+> (rhs >>^ Right)) >>> (zeroArrow ||| id))
    -- no-effect before rhs when lhs fail; unlike catchA
    (<$+>) :: arr i o -> arr i o -> arr i o
    default (<$+>)
        :: (OpDoOrNopA arr, ArrowChoice arr, OpGetSS arr)
        => arr i o -> arr i o -> arr i o
    {- error:
    lhs <$+> rhs = case getSS lhs of
        Chain2 ss tail -> do_or_nopEA ss >>> (rhs ||| tail)
    -}
    lhs <$+> rhs = case (getSS lhs, getSS rhs) of
        (Chain2 h1 t1, Chain2 h2 t2) ->
            setSS (do_or_nopEA h1 >>> left h2) (t2 ||| t1)
class OpDoOrNopA arr => IArrowBiasedPlus arr where
    -- proof: IArrowBiasedPlus is OpDoOrNopA
    --      do_or_nopA a = (a >>> arr Just) <<+> constA Nothing
    -- proof: OpDoOrNopA+ArrowChoice is IArrowBiasedPlus
    --      see default implement of <<+>
    -- no-effect before rhs when lhs fail; unlike catchA
    (<<+>) :: arr i o -> arr i o -> arr i o
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
    default (<<+>) :: ArrowChoice arr => arr i o -> arr i o -> arr i o
    f <<+> g = do_or_nopEA f >>> (g ||| id)
infixr 5 <<+>

instance IArrowBiasedPlus arr => OpArrowPlusBy ByArrowBiasedPlus arr where
    plusBy_ = (<<+>)
instance IArrowBiasedPlusSS arr => OpArrowPlusBy ByArrowBiasedPlusSS arr where
    plusBy_ = (<$+>)


--}
--}
--}
--}
--}
