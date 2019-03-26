{-# LANGUAGE TypeSynonymInstances #-}
    -- for "JSON String"

module JSONClass
    ( JAry(fromJAry)
    , jary
    , JObj(fromJObj)
    , jobj
    , JStr(fromJStr)
    , jstr
    , JValue(..)
    , JSONError
    , JSON(..)
    ) where

import Control.Arrow (second)




newtype JAry a = JAry {fromJAry :: [a]}
    deriving (Eq, Ord, Show)
jary :: [a] -> JAry a
jary = JAry


newtype JObj a = JObj {
    fromJObj :: [(String, a)]
    } deriving (Eq, Ord, Show)
jobj = JObj

newtype JStr = JStr {fromJStr :: String}
    deriving (Eq, Ord, Show)
jstr = JStr



data JValue = JString JStr -- was String
    | JNumber Double
    | JBool Bool
    | JNull
    | JObject (JObj JValue) -- was [(String, JValue)]
    | JArray  (JAry JValue) -- was [JValue]
    deriving (Eq, Ord, Show)


type JSONError = String
class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a
instance JSON JValue where
    toJValue = id
    fromJValue = Right
instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _ = Left "not a JSON boolean"
instance JSON JStr where
    toJValue = JString
    fromJValue (JString s) = Right s
    fromJValue _ = Left "not a JSON string"

instance JSON Double where
    toJValue = JNumber
    fromJValue (JNumber d) = Right d
    fromJValue _ = Left "not a JSON number"

instance (JSON a) => JSON (JAry a) where
    toJValue = JArray . jary . map toJValue . fromJAry
    fromJValue (JArray ary) = whenRight jary . mapEithers fromJValue . fromJAry $ ary
    fromJValue _ = Left "not a JSON array"

instance (JSON a) => JSON (JObj a) where
    toJValue = JObject . jobj . map (second toJValue) . fromJObj
    fromJValue (JObject obj) = whenRight jobj . mapEithers unwrap . fromJObj $ obj
        where unwrap (k,v) = whenRight ((,) k) (fromJValue v)
    fromJValue _ = Left "not a JSON object"

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a) = Right (f a)


mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
    Left err -> Left err
    Right ys -> case f x of
        Left err -> Left err
        Right y -> Right (y:ys)
mapEithers _ _ = Right []

