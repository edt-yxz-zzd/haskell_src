
module Seed.UnsafeUnpack where


unsafe_unjust :: Maybe a -> a
unsafe_unjust (Just a) = a
unsafe_unjust _ = error "unsafe_unjust Nothing"
unsafe_unJust :: Maybe a -> a
unsafe_unJust = unsafe_unjust


unsafe_unLeft :: Either a b -> a
unsafe_unLeft e = case e of
    Left a -> a
    Right _ -> error "unsafe_unLeft fail"
unsafe_unRight :: Either a b -> b
unsafe_unRight e = case e of
    Right a -> a
    Left _ -> error "unsafe_unRight fail"

unsafe_unright :: Either String a -> a
unsafe_unright e = case e of
    Right a -> a
    Left err -> error err

