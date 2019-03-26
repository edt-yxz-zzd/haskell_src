{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DefaultSignatures #-}



{-
    like Arrow, but no constructor
    Combinatory logic
-}


module Lambda.CombinatoryLogic
    ( CategoryF (..)
    , (>>>), (<<<)
    , CombinatoryLogic (..)
    , (<@>)
    , define_S_from_BCKW
    , define_I_from_BCKW
    , define_B_from_SKI
    , define_C_from_SKI
    , define_W_from_SKI
    , _I_, _S_, _K_, _K2_, _B_, _C_, _W_
    , _I, _S, _K, _K2, _B, _C, _W
    , _2S, _2B
    -- , _apply_ex_, _const_, _const2_, _
    , apply_ex, const, const2, composition, flip, double
    , CombinatoryLogicData (..)
    , eval, eval_nonAtom, eval_Atom
    )
where

import qualified Prelude as P
import Prelude (($), (.), Monad(..))
-- import Control.Category
import qualified Data.Function as DF


type Arr3 arr c b a = arr c (arr b a)
type Arr4 arr d c b a = arr d (Arr3 arr c b a)
type Arr5 arr e d c b a = arr e (Arr4 arr d c b a)
type family Arr (arr :: * -> * -> *) a :: *
type instance Arr arr (a->b) = arr (Arr arr a) (Arr arr b)
type instance Arr arr (a, b) = arr a b
type instance Arr arr [a] = a
type Arr6W arr f e d c b a = Arr arr (f->e->d->c->b->a)
    --((f1,f2) -> (e1,e2) -> (d1,d2) -> (c1,c2) -> (b1,b2) -> (a1,a2))
type Arr6 arr f e d c b a = Arr arr ([f] -> [e] -> [d] -> [c] -> [b] -> [a])


newtype Rec arr a = Rec (arr (Rec arr a) a)

class CategoryF arr where
    _id_ :: (a ~ arr a1 a2) => arr a a
    (<*>)
        :: (a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
        => arr b c -> arr a b -> arr a c

    default _id_
        :: (CombinatoryLogic arr, a ~ arr a1 a2) => arr a a
    default (<*>)
        :: (CombinatoryLogic arr, a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
        => arr b c -> arr a b -> arr a c
    -- (<*>) = apply . apply _B_
    (<*>) = _2B
    -- _id_ = _W _K_
    _id_ = define_I_from_BCKW
    {-# MINIMAL _id_ | (<*>) #-}


(>>>)
    :: (CategoryF arr, a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
    => arr a b -> arr b c -> arr a c
infixr 1 >>>
(>>>) = P.flip (<*>)
(<<<)
    :: (CategoryF arr, a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
    => arr b c -> arr a b -> arr a c
infixr 1 <<<
(<<<) = (<*>)



-- class Category arr => CombinatoryLogic arr where
class CategoryF arr => CombinatoryLogic arr where
    -- using (arr a a') as basic object instead of a
    -- to support bijection
    --      ==>> no (arr :: (a->b) -> arr a b), since it has no reversor
    --      ==>> no Y / fix
    -- S K I C apply

    -- I :: a -> a
    -- id :: ((-->) ~ arr) => a --> a
    -- id :: arr a a
    -- _id_ :: arr (arr a a') (arr a a')
    -- _id_ :: (a ~ arr a1 a2) => arr a a
    -- S :: (a->(b->c)) -> (a->b) -> (a->c)
    -- _apply_ex_ :: arr (arr a (arr b c)) (arr (arr a b) (arr a c))
    -- S :: (a->b->c) -> ((a->b) -> a -> c)
    -- _apply_ex_ :: arr (Arr3 arr a b c) (Arr3 arr (arr a b) a c)
    _apply_ex_
        :: (a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
        => arr (Arr3 arr a b c) (Arr3 arr (arr a b) a c)
    -- K :: a -> b -> a
    -- K2 :: b -> a -> a
    -- _const_ :: Arr3 arr a b a
    -- _const2_ :: Arr3 arr b a a
    _const_
        :: (a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
        => Arr3 arr a b a
    _const2_
        :: (a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
        => Arr3 arr b a a
    -- C :: (a -> b -> c) -> (b -> a -> c)
    -- _flip_ :: arr (Arr3 arr a b c) (Arr3 arr b a c)
    _flip_
        :: (a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
        => arr (Arr3 arr a b c) (Arr3 arr b a c)
    -- B :: (b->c) -> (a->b) -> (a->c)
    -- B = (.) -- (.) is (->) not arr
    -- (<*>) = apply B
    -- _composition_ :: Arr3 arr (arr b c) (arr a b) (arr a c)
    _composition_
        :: (a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
        => Arr3 arr (arr b c) (arr a b) (arr a c)
    -- W :: (a->a->b) -> (a->b)
    -- _double_ :: arr (Arr3 arr a a b) (arr a b)
    _double_
        :: (a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
        => arr (Arr3 arr a a b) (arr a b)

    -- ($) :: (a->b) -> a -> b
    -- (a->b) -> (()->a) -> (()->b)
    -- apply :: arr a b -> a -> b
    apply
        :: (a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
        => arr a b -> a -> b
    -- apply :: arr (arr a a') (arr b b') -> arr a a' -> arr b b'



    _const2_ = apply _const_ _id_
    {-
        [S K] ==>> [B C K W]
            B = S (K S) K
            C = S (S (K (S (K S) K)) S) (K K)
            K = K
            W = S S (S K)
        [B C K W] ==>> [S K I]
            I = W K
            K = K
            S = B (B (B W) C) (B B) = B (B W) (B B C)
        Y = S (K (S I I)) (S (S (K S) K) (K (S I I)))
    -}
    {-
    _composition_ = _2S (_K _S_) _K_
    _double_ = _2S _S_ (_S _K_)
    _flip_ = _2S (_2S (_K (_2S (_K _S_) _K_)) _S_) (_K _K_)
    _apply_ex_ = _2B (_B _W_) (_2B _B_ _C_)
    -}
    _composition_ = define_B_from_SKI
    _double_ = define_W_from_SKI
    _flip_ = define_C_from_SKI
    _apply_ex_ = define_S_from_BCKW
    {-# MINIMAL apply, _const_
      , ((_apply_ex_) | (_composition_, _flip_, _double_)) #-}

define_I_from_BCKW = _W _K_
define_S_from_BCKW = _2B (_B _W_) (_2B _B_ _C_)
define_B_from_SKI = _2S (_K _S_) _K_
define_W_from_SKI = _2S _S_ (_S _K_)
define_C_from_SKI = _2S (_2S (_K (_2S (_K _S_) _K_)) _S_) (_K _K_)



class CombinatoryLogic arr => OperatorY arr where
    -- Y :: (a->a) -> a
    -- Y :: ((a->b) -> (a->b)) -> (a->b)
    -- _fix_ :: arr (arr a a) a
    -- Y :: ((a->b) -> a->b) -> a->b
    -- _fix_ :: Arr3 arr (Arr3 arr (arr a b) a b) a b
    _fix_ :: (a ~ arr a1 a2) => arr (arr a a) a
    -- cannot construct the infinite type: b0 ~ arr b0 a
    -- _fix_ = _2S (_K (_2S id id)) (_2S (_2S (_K _S_) _K_) (_K (_2S id id)))
    -- _fix_ :: Arr3 arr (Arr3 arr (arr a b) a b) a b
    -- apply _fix_ :: (Arr3 arr (arr a b) a b) -> arr a b
    {-target :: (arr a b -> arr a b) -> arr a b
    target = apply _fix_ . arr
    -}
    {-
    arr :: (a->b) -> arr a b
    _fix_ = arr fix where
        fix a2a = let a = apply a2a a in a
    -}

-- (<@>) :: CombinatoryLogic arr => arr a b -> a -> b
(<@>)
    :: (CombinatoryLogic arr, a ~ arr a1 a2, b ~ arr b1 b2)
    => arr a b -> a -> b
(<@>) = apply
infixl 9 <@>



_I, id
    :: (CombinatoryLogic arr, a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
    => a -> a
-- _I = P.id

_I_, define_I_from_BCKW
    :: (CombinatoryLogic arr, a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
    => arr a a

_2S
    :: (CombinatoryLogic arr, a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
    => (Arr3 arr a b c) -> (arr a b) -> arr a c
_S, apply_ex
    :: (CombinatoryLogic arr, a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
    => (->) (Arr3 arr a b c) (Arr3 arr (arr a b) a c)
_S_, define_S_from_BCKW
    :: (CombinatoryLogic arr, a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
    => arr (Arr3 arr a b c) (Arr3 arr (arr a b) a c)
_K, const
    :: (CombinatoryLogic arr, a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
    => a -> arr b a
_K_
    :: (CombinatoryLogic arr, a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
    => Arr3 arr a b a
_W, double
    :: (CombinatoryLogic arr, a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
    => (->) (Arr3 arr a a b) (arr a b)
_W_, define_W_from_SKI
    :: (CombinatoryLogic arr, a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
    => arr (Arr3 arr a a b) (arr a b)
_K2, const2
    :: (CombinatoryLogic arr, a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
    => a -> arr b b
_K2_
    :: (CombinatoryLogic arr, a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
    => Arr3 arr a b b
_C, flip
    :: (CombinatoryLogic arr, a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
    => (->) (Arr3 arr a b c) (Arr3 arr b a c)
_C_, define_C_from_SKI
    :: (CombinatoryLogic arr, a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
    => arr (Arr3 arr a b c) (Arr3 arr b a c)
_2B
    :: (CombinatoryLogic arr, a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
    => (arr b c) -> (arr a b) -> (arr a c)
_B, composition
    :: (CombinatoryLogic arr, a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
    => (arr b c) -> arr (arr a b) (arr a c)
_B_, define_B_from_SKI
    :: (CombinatoryLogic arr, a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
    => Arr3 arr (arr b c) (arr a b) (arr a c)
id = apply _id_
apply_ex = apply _apply_ex_
const = apply _const_
const2 = apply _const2_
flip = apply _flip_
double = apply _double_
composition = apply _composition_
_I = id
_2S = apply P.. _S
_S = apply_ex
_K = const
_K2 = const2
_C = flip
_W = double
_2B = (<*>)
_B = composition



_I_ = _id_
_S_ = _apply_ex_
_K_ = _const_
_K2_ = _const2_
_C_ = _flip_
_W_ = _double_
_B_ = _composition_



_Y_ :: (OperatorY arr, a ~ arr b c) => arr (arr a a) a
_Y_ = _fix_
_Y = fix
fix = apply _fix_
_Y, fix :: (OperatorY arr, a ~ arr b c) => (->) (arr a a) a

{-
class OperatorY arr_ () => Abstraction arr_ where
    -- \x -> expr
    -- arr_ frees in out
    abstract :: Arr3 (arr_ ()) (arr_ (x, xs) a b) x (arr_ xs a b)
-}









-----------------------------------------------------------------







------------------------------------------


data CombinatoryLogicData tpl a b where
    -- CLD_I :: (arr ~ CombinatoryLogicData tpl, a ~ b) => arr a a
    CLD_I
        :: (arr ~ CombinatoryLogicData tpl, a ~ b, a ~ arr a1 a2)
        => CombinatoryLogicData tpl a a
    CLD_K
        :: (arr ~ CombinatoryLogicData tpl, a ~ arr a1 a2, b ~ arr b1 b2)
        => Arr3 (CombinatoryLogicData tpl) a b a
    CLD_S
        :: (arr ~ CombinatoryLogicData tpl, a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
        => Arr3 (CombinatoryLogicData tpl) (Arr3 arr a b c) (arr a b) (arr a c)
    CLD_apply
        :: (arr ~ CombinatoryLogicData tpl, a ~ arr a1 a2, b ~ arr b1 b2, c ~ arr c1 c2)
        -- => arr a b -> a -> b
        => arr a b -> a -> CombinatoryLogicData tpl b1 b2
    CLD_Atom :: tpl a b -> CombinatoryLogicData tpl a b
    -- CLD_Atom_Composition :: tpl b c -> tpl a b -> CombinatoryLogicData a c
{-
data CombinatoryLogicData tpl a b
    = CLD_I | CLD_K | CLD_S
    | forall x1 x2. CLD_apply
        (CombinatoryLogicData tpl
            (CombinatoryLogicData tpl a b)
            (CombinatoryLogicData tpl x1 x2)
        )
        (CombinatoryLogicData tpl x1 x2)
    | CLD_Atom (tpl a b)
-}

--instance CategoryF tpl => CategoryF (CombinatoryLogicData tpl) where
instance CategoryF (CombinatoryLogicData tpl) where
    _id_ = CLD_I
    -- (<*>) = apply . apply _B_
    -- (<*>) = _2B
--instance CategoryF tpl => CombinatoryLogic (CombinatoryLogicData tpl) where
instance CombinatoryLogic (CombinatoryLogicData tpl) where
    _apply_ex_ = CLD_S
    _const_ = CLD_K
    apply = CLD_apply



eval_nonAtom :: Monad m => CombinatoryLogicData tpl a b -> m (a->b)
eval_nonAtom g2h = eval g2h err P.return where
    err = P.const $ fail "CombinatoryLogicData is (CLD_Atom ...)"
eval_Atom :: Monad m => CombinatoryLogicData tpl a b -> m (tpl a b)
eval_Atom tpl = eval tpl P.return err where
    err = P.const $ fail "CombinatoryLogicData is not (CLD_Atom ...)"


eval
    :: (CombinatoryLogicData tpl ~ arr)
    => arr a b
    -- -> (tpl b c -> tpl a b -> tpl a c)
    -> (tpl a b -> x)
    -> ((a->b) -> x)
    -> x
eval cld fa ff = _eval cld
  where
    _eval_nonAtom :: CombinatoryLogicData tpl a b -> (a->b)
    _eval_nonAtom g2h = eval g2h P.undefined P.id
    _eval cld = case cld of
        CLD_Atom a -> fa a
        CLD_I -> ff _I
        CLD_S -> ff _S
        CLD_K -> ff _K
        CLD_apply g2h g -> _eval (_eval_nonAtom g2h $ g)


--}
--}
--}
--}
--}

