
module MapEx
    ( alterXO
    , alterXX
    , alterXV
    , alterXC

    , alterOOVO
    , alterOOVX
    , alterOOVV
    , alterOOVI
    , alterOOVC

    , alterOXVO
    , alterOXVX
    , alterOXVV
    , alterOXVI
    , alterOXVC

    , alterOCVO
    , alterOCVX
    , alterOCVV
    , alterOCVI
    , alterOCVC
    )
where

import Data.Map as M

{-
    alter :: (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
    Nothing, Maybe a, Just a, const a == O, X, V, C
    X -> O | X | V | C                          -- 4
    O -> O | X | C, V -> O | X | V | I | C  -- 3*5
    total 19!
    M -- without value
    M -> O | C <==> X -> O | C
    K -- with key
    K X -> O | X | ...
    total: ??
    -- below3 == ~, a->a==id
    () -> ():           nop
    () -> Maybe a:      add or nop
    () -> a:            add
    --
    Maybe a -> ():      delete or nop
    Maybe a -> Maybe a: alter
        == a->Maybe a, ()->Maybe a
    Maybe a -> a:       add or adjust -- assign
    -- below3 == ~, ()->()
    a -> ():            delete
    a -> Maybe a:       delete or adjust
    a -> a:             adjust
-}


type A k a = k -> Map k a -> Map k a
alterXO :: Ord k => A k a
alterXO = delete
alterXX :: Ord k => (Maybe a -> Maybe a) -> A k a
alterXX = alter
alterXV :: Ord k => (Maybe a -> a) -> A k a
alterXV f = alter $ Just . f
alterXC :: Ord k => a -> A k a
alterXC = flip insert


alterOOVO :: Ord k => A k a
alterOOVO = alterXO -- same
alterOOVX :: Ord k => (a -> Maybe a) -> A k a
alterOOVX = update
alterOOVV :: Ord k => (a -> a) -> A k a
alterOOVV = adjust
alterOOVI :: Ord k => A k a
alterOOVI = const id -- useless
alterOOVC :: Ord k => a -> A k a
alterOOVC = adjust . const



alterOXVO :: Ord k => Maybe a -> A k a
alterOXVO x = alterOXVX x $ const Nothing
alterOXVX :: Ord k => Maybe a -> (a -> Maybe a) -> A k a
alterOXVX x = alterXX . maybe x
alterOXVV :: Ord k => Maybe a -> (a -> a) -> A k a
alterOXVV x = alterOXVX x . (Just .)
alterOXVI :: Ord k => Maybe a -> A k a
-- alterOXVI x = alterOXVX x id
alterOXVI x k d = if member k d then d else
    alterOXVX x Just k d
    -- alterXX (maybe x Just) k d
alterOXVC :: Ord k => Maybe a -> a -> A k a
alterOXVC x = alterXX . maybe x . const . Just



alterOCVO :: Ord k => a -> A k a
alterOCVO a = alterXX $ maybe (Just a) (const Nothing)
alterOCVX :: Ord k => a -> (a -> Maybe a) -> A k a
alterOCVX a = alterXX . maybe (Just a)
alterOCVV :: Ord k => a -> (a -> a) -> A k a
alterOCVV a = alterXX . maybe (Just a) . (Just .)
alterOCVI :: Ord k => a -> A k a
alterOCVI a k d = if member k d then d else insert k a d
alterOCVC :: Ord k => a -> a -> A k a
alterOCVC a = alterXX . maybe (Just a) . const . Just


