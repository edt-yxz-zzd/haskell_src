
module ReadHelper__UInt (readsPrec_viaUInt) where


newtype UInt = UInt Integer
    deriving (Read)

readsPrec_viaUInt :: (Integer -> a) -> Int -> ReadS a
readsPrec_viaUInt f d s = do
    (UInt i, s') <- readsPrec d s
    return (f i, s')


