{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bijection.BijectionEx
    ( module Bijection.Bijection
    , module Bijection.BiArrow
    )
where


import Bijection.Bijection
import Bijection.BiArrow hiding ((.))


-- pair
-- either


data SwapPair a b
instance Bijection (SwapPair a b) where
    type BijectionFrom (SwapPair a b) = (a, b)
    type BijectionTo (SwapPair a b) = (b, a)
    forward _ (a, b) = (b, a)
    backward _ (b, a) = (a, b)
_swap :: BijectionObj (a,b) (b,a)
_swap = make_bijection_obj (undefined :: SwapPair a b)
data SwapEither a b
instance Bijection (SwapEither a b) where
    type BijectionFrom (SwapEither a b) = Either a b
    type BijectionTo (SwapEither a b) = Either b a
    forward _ (Left a) = Right a
    forward _ (Right b) = Left b
    backward _ (Right a) = Left a
    backward _ (Left b) = Right b
_swap_either :: BijectionObj (Either a b) (Either b a)
_swap_either = make_bijection_obj (undefined :: SwapEither a b)







data PairBijection a b = PairBijection a b

instance (Bijection a, Bijection b)
    => Bijection (PairBijection a b) where
    type BijectionFrom (PairBijection a b) = (BijectionFrom a, BijectionFrom b)
    type BijectionTo (PairBijection a b) = (BijectionTo a, BijectionTo b)
    forward (PairBijection a2b c2d) (a, c) = (forward a2b a, forward c2d c)
    backward (PairBijection a2b c2d) (b, d) = (backward a2b b, backward c2d d)
make_pair_bijection
    :: (Bijection a, Bijection b)
    => a -> b
    -> BijectionObj (BijectionFrom a, BijectionFrom b)
                    (BijectionTo a, BijectionTo b)
make_pair_bijection a = make_bijection_obj . PairBijection a




data EitherBijection a b = EitherBijection a b

instance (Bijection a, Bijection b)
    => Bijection (EitherBijection a b) where
    type BijectionFrom (EitherBijection a b) =
            Either (BijectionFrom a) (BijectionFrom b)
    type BijectionTo (EitherBijection a b) =
            Either (BijectionTo a) (BijectionTo b)
    forward (EitherBijection a2b c2d) a_or_c = case a_or_c of
        Left a -> Left $ forward a2b a
        Right c -> Right $ forward c2d c
    backward (EitherBijection a2b c2d) b_or_d = case b_or_d of
        Left b -> Left $ backward a2b b
        Right d -> Right $ backward c2d d
make_either_bijection
    :: (Bijection a, Bijection b)
    => a -> b
    -> BijectionObj (Either (BijectionFrom a) (BijectionFrom b))
                    (Either (BijectionTo a) (BijectionTo b))
make_either_bijection a = make_bijection_obj . EitherBijection a


data Merge a
instance Bijection (Merge a) where
    type BijectionFrom (Merge a) = Either a a
    type BijectionTo (Merge a) = (Bool, a)
    forward _ (Left a) = (False, a)
    forward _ (Right a) = (True, a)
    backward _ (False, a) = Left a
    backward _ (True, a) = Right a
_merge = make_bijection_obj (undefined :: Merge a)



data SecondEx c a = SecondEx (c -> a)
instance Bijection a => Bijection (SecondEx c a) where
    type BijectionFrom (SecondEx c a) = (c, BijectionFrom a)
    type BijectionTo (SecondEx c a) = (c, BijectionTo a)
    forward (SecondEx c2a2b) (c, a) = (c, forward (c2a2b c) a)
    backward (SecondEx c2a2b) (c, b) = (c, backward (c2a2b c) b)
_second_ex
    :: Bijection a
    => (c -> a) -> BijectionObj (c, BijectionFrom a) (c, BijectionTo a)
_second_ex c2a = make_bijection_obj (SecondEx c2a)
instance BiArrow BijectionObj where
    (***) = make_pair_bijection
    swap_pair = _swap
    second_ex = _second_ex

instance BiArrowChoice BijectionObj where
    (+++) = make_either_bijection
    a2d ||| c2d = (a2d +++ c2d) >>> _merge
    swap_either = _swap_either


