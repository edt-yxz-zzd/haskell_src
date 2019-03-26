{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}


{-
-- system F
type UInt = Maybe PInt
type PInt = List Bit
type SizedList a = ADT exists ls. {uncons :: ls -> Maybe (Pair a ls), size :: ls -> UInt, empty :: ls, cons :: a -> ls -> ls, foldr :: (a->Will r->r) -> r->r}
type Seq a = ADT exists seq. {unfinger :: seq a ->  }



-}
import Data.Proxy
type P = Proxy
p :: forall a. P a
p = Proxy

--type Exists = P r -> (P a -> a->r) -> r
type Exists r = (forall a. P a -> a->r) -> r

type Will r = () -> r
will :: P r -> r -> (()->r)
will _ = const
get :: P r -> (()->r) -> r
get _ f = f ()
-- type Forall a = P r -> a r
type Pair_ r a b = (a->b->r)->r
type Pair a b = P r -> Pair_ r a b
mk_pair :: a -> b -> Pair a b
mk_pair a b ab2r = ab2r a b
fst_ :: forall a b. Pair a b -> a
snd_ :: forall a b. Pair a b -> b
fst_ p = p (\a b->a)
snd_ p = p (\a b->b)

{-
type CBool_ r = r -> r -> r -- true, false
type CBool = P r -> CBool_ r
true, false :: CBool
true = const
false = const id
if_ :: P a -> CBool -> a -> a -> a
if_ = id

type CMaybe_ r a = (a->r) -> r->r
type CMaybe a = P r -> CMaybe_ r a
nothing :: CMaybe a
nothing = const id
just :: P a -> a -> CMaybe a
just a f _ = f a
cmaybe_fmap :: P a b. (a->b) -> CMaybe a -> CMaybe b
cmaybe_fmap f m = m (\a-> just (f a)) nothing

type CUInt_ r = (Will r->r) -> r->r
type CUInt = P r -> CUInt_ r
cuint_zero :: CUInt
--cuint_succ :: CUInt -> CUInt
cuint_succ :: P r -> CUInt_ r -> CUInt_ r
cuint_zero = const id
cuint_succ u f r = f (will (u f r))
{-
cuint_pred u = u succ nothing where
    succ :: Will (CMaybe CUInt) -> CMaybe CUInt
    succ m = (get m) (cmaybe_fmap cuint_succ) (just cuint_zero)
_msucc :: Will (CMaybe (CUInt)) -> CMaybe (CUInt)
_msucc m = (get (m @(CMaybe CUInt))) (\u-> just (cuint_succ u)) (just cuint_zero)
_msucc :: P rm ru. Will (CMaybe CUInt) -> CMaybe_CUInt_ rm ru
_msucc m = m () @(CMaybe_CUInt_ rm ru) (\u-> just (cuint_succ u)) (just cuint_zero)
-}
--_msucc :: P rm ru. Will (CMaybe CUInt) -> (CMaybe_ rm (CUInt_ ru))
type CMaybe_CUInt_ rm ru = CMaybe_ rm (CUInt_ ru)
--_msucc :: P rm ru. Will (CMaybe CUInt) -> CMaybe_CUInt_ rm ru
--why fail: _msucc :: Will (CMaybe CUInt) -> CMaybe CUInt
_msucc :: P rm ru. Will (CMaybe_CUInt_ (CMaybe_CUInt_ rm ru) ru) -> CMaybe_CUInt_ rm ru
_msucc m = m () (\u-> just (cuint_succ u)) (just cuint_zero)
data Tmp_msucc = Tmp_msucc {unTmp_msucc :: Will (CMaybe CUInt) -> CMaybe CUInt}
data Tmp_maybe_uint = Tmp_maybe_uint {unTmp_maybe_uint :: CMaybe CUInt}


cuint_pred :: CUInt -> CMaybe CUInt
-- cuint_pred u = u _msucc nothing where
--cuint_pred :: P rm ru. CUInt_ (CMaybe_CUInt_ rm ru) -> CMaybe_CUInt_ rm ru
cuint_pred u = unTmp_maybe_uint (u tmp_msucc (Tmp_maybe_uint nothing)) where
    tmp_msucc :: Will Tmp_maybe_uint -> Tmp_maybe_uint
    tmp_msucc will_tmp_m = Tmp_maybe_uint (_msucc . will . unTmp_maybe_uint $ get will_tmp_m)
    --nothing_CUInt :: CMaybe CUInt
    nothing_CUInt :: P rm ru. CMaybe_CUInt_ rm ru
    nothing_CUInt = 

type CList_ r a = (a-> Will r ->r) -> r->r
type CList a = P r -> CList_ r a
clist_empty :: P a -> CList a
clist_empty = const id
clist_cons :: P a -> a -> CList a -> CList a
clist_cons h ts ar2r r = ar2r h (\()->ts ar2r r)



{-
type BinPInt = CList CBool
    -- little-endian
    -- [A,B,C] ==>> "0b1CBA"
binpint_1, binpint_2, binpint_3 :: BinPInt
binpint_1 = clist_empty
binpint_2 = clist_cons @CBool false binpint_1
binpint_3 = clist_cons true binpint_1
binpint_4 = clist_cons false binpint_2
binpint_5 = clist_cons true binpint_2
binpint_succ :: BinPInt -> BinPInt
binpint_succ p = fst_
    (p (\bit p1_p ->mk_pair
                    (will (if_ bit
                        {- 1:p -> 0:p+1 -}(clist_cons false (fst_ (get p1_p)))
                        {- 0:p -> 1:p   -}(clist_cons true  (snd_ (get p1_p)))
                        )
                    )
                    (will {- b:p -} (clist_cons bit (snd_ (get p1_p))))
                    )
        (mk_pair (will binpint_2) (will binpint_1))
    )




--}
--}
--}
--}

