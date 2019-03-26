{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Seed.Loop
    ( loop, loop_ex
    , loop2, loop2_mutual_one_main, loop2_mutual_recursion
    , recur, recur2
    )
where

import Data.List
import Data.Maybe
import Control.Arrow hiding (loop)

-- List.unfoldr
{-
loop
    :: (global_config -> local_mutable -> Either result local_mutable)
    -> (global_config -> local_mutable -> result)
loop step = loop_ . step
-}


loop
    :: (local_mutable -> Either result local_mutable)
    -> (local_mutable -> result)
loop step = f where
    f = either id f . step

loop_ex
    :: (local_mutable -> a)
    -> (a -> Maybe local_mutable)
    -> (local_mutable -> [a])
loop_ex step extract = f where
    f local_mutable = h:ts where
        h = step local_mutable
        ts = maybe [] f $ extract h


loop2
    :: (local_mutable -> local_mutable)
    -> (local_mutable -> Maybe result)
    -> (local_mutable -> result)
loop2 f g = head . catMaybes . map g . iterate f

loop2_mutual_recursion
    :: (a -> Either a b)
    -> (b -> Either a b)
    -> (Either a b -> Maybe result)
    -> (Either a b -> result)
loop2_mutual_recursion f g = loop2 $ f ||| g


loop2_mutual_one_main
    :: (a -> Either a b)
    -> (b -> Either a b)
    -> (a -> Maybe result)
    -> (a -> result)
loop2_mutual_one_main f g h =
    loop2_mutual_recursion f g (h ||| const Nothing) . Left




--------------------------
recur
    :: (Either input (locals, result) -> Either result (locals, input))
    -> (input -> result)
recur fg i = loop step local_mutable where
    {-
    local_mutable = (Either input (locals, result), stack)
    stack = [locals]
    step :: local_mutable -> Either result local_mutable
    -}
    local_mutable = (Left i, [])
    step (i, stack) = case fg i of
        Left result -> case stack of
            h:ts -> Right (Right (h, result), ts)
            [] -> Left result
        Right (locals, input) -> Right (Left input, locals : stack)


data FF result where
    -- recur call
    FF2 :: i -> F i x -> F x result -> FF result
    -- loop
    FF1 :: i -> F i result -> FF result
    -- return
    FF0 :: result -> FF result
newtype F input result = F { callF :: input -> FF result }
data Stack input result where
    Empty :: Stack result result
    Cons :: F input x -> Stack x result -> Stack input result
data Local result where
    Local :: input -> F input x -> Stack x result -> Local result

recur2 :: forall input result . (F input result) -> (input -> result)
recur2 i2r i = loop step local_mutable where
    --local_mutable = ((i, i2x), Stack x result)
    local_mutable = Local i i2r Empty
    step :: forall . Local result -> Either result (Local result)
    step (Local i i2x stack_x2r) = case callF i2x i of
        FF0 x -> case stack_x2r of
            Empty -> Left x
            Cons x2y stack_y2r -> Right $ Local x x2y stack_y2r
        FF2 i' i2y y2x -> Right . Local i' i2y $ Cons y2x stack_x2r
        FF1 i' i2x'    -> Right $ Local i' i2x' stack_x2r



