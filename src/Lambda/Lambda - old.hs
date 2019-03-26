{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}



{-
    like Arrow, but no constructor
    Combinatory logic
-}


import qualified Prelude as P
import Prelude (($))
import Control.Category
import qualified Data.Function as DF


type Arr3 arr c b a = arr c (arr b a)
type Arr4 arr d c b a = arr d (Arr3 arr c b a)
type Arr5 arr e d c b a = arr e (Arr4 arr d c b a)


newtype Rec arr a = Rec (arr (Rec arr a) a)

class Category arr => CombinatoryLogic arr where
    -- to support bijection
    --      ==>> no (arr :: (a->b) -> arr a b), since it has no reversor
    --      ==>> no Y / fix
    -- S K I C apply

    -- I :: a -> a
    -- id :: ((-->) ~ arr) => a --> a
    -- id :: arr a a
    -- S :: (a->(b->c)) -> (a->b) -> (a->c)
    -- _apply_ex_ :: arr (arr a (arr b c)) (arr (arr a b) (arr a c))
    -- S :: (a->b->c) -> ((a->b) -> a -> c)
    _apply_ex_ :: arr (Arr3 arr a b c) (Arr3 arr (arr a b) a c)
    -- K :: a -> b -> a
    _const_ :: Arr3 arr a b a
    _const2_ :: Arr3 arr a b b
    -- C :: (a -> b -> c) -> (b -> a -> c)
    _flip_ :: arr (Arr3 arr a b c) (Arr3 arr b a c)
    -- B :: (b->c) -> (a->b) -> (a->c)
    -- B = (.) -- (.) is (->) not arr
    _composition_ :: Arr3 arr (arr b c) (arr a b) (arr a c)
    -- W :: (a->a->b) -> (a->b)
    _double_ :: arr (Arr3 arr a a b) (arr a b)

    -- ($) :: (a->b) -> a -> b
    -- (a->b) -> (()->a) -> (()->b)
    apply :: arr a b -> a -> b



    _const2_ = const id
    {-
        [B C K W] ==>> [S K I]
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
    _composition_ = _2S (_K _S_) _K_
    _double_ = _2S _S_ (_S _K_)
    _flip_ = _2S (_2S (_K (_2S (_K _S_) _K_)) _S_) (_K _K_)
    -- _apply_ex_ = _B (_B _W_) <@> (_B _B_ <@> _C_)
    _apply_ex_ = (_B _W_) . _B_ . _C_

class CombinatoryLogic arr => OperatorY arr where
    -- Y :: (a->a) -> a
    -- Y :: ((a->b) -> (a->b)) -> (a->b)
    -- _fix_ :: arr (arr a a) a
    -- Y :: ((a->b) -> a->b) -> a->b
    -- _fix_ :: Arr3 arr (Arr3 arr (arr a b) a b) a b
    _fix_ :: (a ~ arr b c) => arr (arr a a) a
    -- cannot construct the infinite type: b0 ~ arr b0 a
    -- _fix_ = _2S (_K (_2S id id)) (_2S (_2S (_K _S_) _K_) (_K (_2S id id)))
    -- _fix_ :: Arr3 arr (Arr3 arr (arr a b) a b) a b
    -- apply _fix_ :: (Arr3 arr (arr a b) a b) -> arr a b
    {-target :: (arr a b -> arr a b) -> arr a b
    target = apply _fix_ . arr
    -}
    arr :: (a->b) -> arr a b
    _fix_ = arr fix where
        fix a2a = let a = apply a2a a in a

(<@>) :: CombinatoryLogic arr => arr a b -> a -> b
(<@>) = apply
infixl 9 <@>



_I_ :: CombinatoryLogic arr => arr a a
_I_ = id

_2S :: CombinatoryLogic arr => (Arr3 arr a b c) -> (arr a b) -> arr a c
_S, apply_ex :: CombinatoryLogic arr => (->) (Arr3 arr a b c) (Arr3 arr (arr a b) a c)
_S_ :: CombinatoryLogic arr => arr (Arr3 arr a b c) (Arr3 arr (arr a b) a c)
_K, const :: CombinatoryLogic arr => a -> arr b a
_K_ :: CombinatoryLogic arr => Arr3 arr a b a
_W, double :: CombinatoryLogic arr => (->) (Arr3 arr a a b) (arr a b)
_W_ :: CombinatoryLogic arr => arr (Arr3 arr a a b) (arr a b)
_K2, const2 :: CombinatoryLogic arr => a -> arr b b
_K2_ :: CombinatoryLogic arr => Arr3 arr a b b
_C, flip :: CombinatoryLogic arr => (->) (Arr3 arr a b c) (Arr3 arr b a c)
_C_ :: CombinatoryLogic arr => arr (Arr3 arr a b c) (Arr3 arr b a c)
_2B :: CombinatoryLogic arr => (arr b c) -> (arr a b) -> (arr a c)
_B, composition :: CombinatoryLogic arr => (arr b c) -> arr (arr a b) (arr a c)
_B_ :: CombinatoryLogic arr => Arr3 arr (arr b c) (arr a b) (arr a c)
apply_ex = apply _apply_ex_
const = apply _const_
const2 = apply _const2_
flip = apply _flip_
double = apply _double_
composition = apply _composition_
_2S = apply . _S
_S = apply_ex
_K = const
_K2 = const2
_C = flip
_W = double
_2B = (.)
_B = composition



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





class Bijection a where
    type BijectionFrom a :: *
    type BijectionTo a :: *
    forward :: a -> BijectionFrom a -> BijectionTo a
    backward :: a -> BijectionTo a -> BijectionFrom a
    bijection_vtable :: BijectionVTable (BijectionFrom a) (BijectionTo a) a
    bijection_vtable = BVT {_forward = forward, _backward = backward}
data BijectionVTable from_ to_ a = BVT
    { _forward :: a -> from_ -> to_
    , _backward :: a -> to_ -> from_
    }
data BijectionObj from_ to_ = forall a. BiObj
    { _obj :: a
    , _vtable :: BijectionVTable from_ to_ a
    }

make_bijection_obj
    :: Bijection a => a -> BijectionObj (BijectionFrom a) (BijectionTo a)
make_bijection_obj a = BiObj {_obj = a, _vtable = bijection_vtable}
instance Bijection (BijectionObj a b) where
    type BijectionFrom (BijectionObj a b) = a
    type BijectionTo (BijectionObj a b) = b
    forward BiObj {_obj = a, _vtable = table} = _forward table a
    backward BiObj {_obj = a, _vtable = table} = _backward table a



data ChainBijection outer inner = CB outer inner
instance (Bijection a, Bijection b, BijectionTo a ~ BijectionFrom b)
    => Bijection (ChainBijection a b) where
    type BijectionFrom (ChainBijection a b) = BijectionFrom a
    type BijectionTo (ChainBijection a b) = BijectionTo b
    forward (CB a b) = forward b . forward a
    backward (CB a b) = backward a . backward b
make_chain_bijection
    :: (Bijection a, Bijection b)
    => a -> b -> ChainBijection a b
make_chain_bijection = CB







data Bijection__id a
instance Bijection (Bijection__id a) where
    type BijectionFrom (Bijection__id a) = a
    type BijectionTo (Bijection__id a) = a
    forward = P.const P.id
    backward = forward

bijection_obj__id :: BijectionObj a a
bijection_obj__composition
    :: (Bijection a, Bijection b, BijectionTo a ~ BijectionFrom b)
    => b -> a -> BijectionObj (BijectionFrom a) (BijectionTo b)
bijection_obj__id = make_bijection_obj (P.undefined :: Bijection__id a)
bijection_obj__composition b a =
    make_bijection_obj P.$ make_chain_bijection a b


instance Category BijectionObj where
    id = bijection_obj__id
    (.) = bijection_obj__composition
instance CombinatoryLogic BijectionObj where
    _apply_ex_ f__ g_ a = !!!!!!!!!!!!!!!!!!!!!


--}
--

