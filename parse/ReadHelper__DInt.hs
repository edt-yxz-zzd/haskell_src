
module ReadHelper__DInt (readsPrec_viaDInt) where


newtype DInt = DInt Integer
    deriving (Read)

readsPrec_viaDInt :: (Integer -> a) -> Int -> ReadS a
readsPrec_viaDInt f d s = do
    (DInt i, s') <- readsPrec d s
    return (f i, s')


