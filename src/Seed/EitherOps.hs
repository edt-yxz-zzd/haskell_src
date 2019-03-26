
module Seed.EitherOps
    ( module Seed.EitherOps
    , module Data.Either
    -- , module Data.Bifunctor
    , bimap -- , fst_map, snd_map
    , module Seed.OpSwap
    )
where

import Data.Either
import Data.Bifunctor as B
import Seed.OpSwap


fst_map :: Bifunctor f => (a->a') -> (f a x) -> (f a' x)
snd_map :: Bifunctor f => (a->a') -> (f x a) -> (f x a')
fst_map = B.first
snd_map = B.second

either_merge, unEither :: Either a a -> a
either_merge = either id id
unEither = either_merge
either2maybe :: Either a b -> Maybe b
either2maybe = either (const Nothing) Just


{- use swap instead
either_flip :: Either a b -> Either b a
either_flip = either Right Left
-}


mapLeft :: (a->b) -> Either a r -> Either b r
mapRight :: (a->b) -> Either e a -> Either e b
mapLeft = flip bimap id
mapRight = fmap


mayLeft :: Either a b -> Maybe a
mayRight :: Either a b -> Maybe b
mayLeft (Left a) = Just a
mayLeft _ = Nothing
mayRight (Right b) = Just b
mayRight _ = Nothing



-- for Arrow
switch :: Bool -> i -> Either i i
switch b = if b then Right else Left


-- withInput (i->eEr) ==>> i->(eEr,i) -->> eiEr
eEr_i_to_eiEr :: (Either e r, i) -> Either (e,i) r
eEr_i_to_eiEr (Left e, i) = Left (e,i)
eEr_i_to_eiEr (Right r, _) = Right r

-- (i->eEx) >>= (x->eEo) ==>> i->eEeEx
e_E_eEr_to_eEr :: Either e (Either e r) -> Either e r
e_E_eEr_to_eEr (Left e) = Left e
e_E_eEr_to_eEr (Right eEr) = eEr
-- catch (eEi->eEo) ==>> (eEi->eE_eEo)
eEr_to_e_E_eEr :: Either e r -> Either e (Either e r)
eEr_to_e_E_eEr = Right

swap_e_E_eEr :: Either e (Either e r) -> Either e (Either e r)
swap_e_E_eEr (Left e) = Right (Left e)
swap_e_E_eEr (Right (Left e)) = Left e
swap_e_E_eEr eEeEr = eEeEr
swap_eEe_E_r :: Either (Either e e) r -> Either (Either e e) r
swap_eEe_E_r = mapLeft swap

a_E_bEc2aEb_E_c :: Either a (Either b c) -> Either (Either a b) c
a_E_bEc2aEb_E_c (Left a) = Left (Left a)
a_E_bEc2aEb_E_c (Right (Left b)) = Left (Right b)
a_E_bEc2aEb_E_c (Right (Right c)) = Right c
aEb_E_c2a_E_bEc :: Either (Either a b) c -> Either a (Either b c)
aEb_E_c2a_E_bEc (Right c) = Right (Right c)
aEb_E_c2a_E_bEc (Left (Right b)) = Right (Left b)
aEb_E_c2a_E_bEc (Left (Left a)) = Left a



