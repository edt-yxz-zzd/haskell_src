
module ReadHelper__PInt (readsPrec_viaPInt) where


newtype PInt = PInt Integer
    deriving (Read)

readsPrec_viaPInt :: (Integer -> a) -> Int -> ReadS a
readsPrec_viaPInt f d s = do
    (PInt i, s') <- readsPrec d s
    return (f i, s')


