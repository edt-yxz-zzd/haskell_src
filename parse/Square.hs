{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

class RectangleConcept a where
    type Width a :: *
    type Height a :: *
class (RectangleConcept a, Width a~Height a)
    => RectangleConcept__SameType a
instance (RectangleConcept a, Width a~Height a)
    => RectangleConcept__SameType a
class RectangleConcept__SameType a => SquareConcept a where
    -- getHeight a === getWidth a
class RectangleConcept a => Rectangle a where
    getWidth :: a -> Width a
    getHeight :: a -> Height a
class (Rectangle a, SquareConcept a) => Square a where
instance (Rectangle a, SquareConcept a) => Square a where

class RectangleConcept__SameType a => OpDynSquare a where
    setBothWidthHeight :: Width a -> a -> a
class RectangleConcept a => OpDynRectangle a where
    setWidth :: Width a -> a -> a
    setHeight :: Height a -> a -> a

class (OpDynSquare a, OpDynRectangle a) => OpDynRectangle__SameType a
    -- setBothWidthHeight e === setWidth e . setHeight e
instance (OpDynSquare a, OpDynRectangle a) => OpDynRectangle__SameType a

class (Rectangle a, OpDynRectangle a) => DynRectangle a
instance (Rectangle a, OpDynRectangle a) => DynRectangle a
class (DynRectangle a, OpDynRectangle__SameType a)
    => DynRectangle__SameType a
instance (DynRectangle a, OpDynRectangle__SameType a)
    => DynRectangle__SameType a
class (Square a, OpDynSquare a) => DynSquare a
instance (Square a, OpDynSquare a) => DynSquare a


class RectangleConcept__SameType a => OpDynMakeSquare a where
    mkSquare :: Width a -> a
class RectangleConcept a => OpDynMakeRectangle a where
    mkRectangle :: (Width a, Height a) -> a












newtype Rect w h = Rect { unRect :: (w, h) }
instance RectangleConcept (Rect w h) where
    type Width (Rect w h) = w
    type Height (Rect w h) = h
instance Rectangle (Rect w h) where
    getWidth = fst . unRect
    getHeight = snd . unRect
instance OpDynRectangle (Rect w h) where
    setWidth w (Rect (_,h)) = Rect (w, h)
    setHeight h (Rect (w,_)) = Rect (w, h)
instance OpDynSquare (Rect w w) where
    setBothWidthHeight w _ = Rect (w, w)
instance OpDynMakeSquare (Rect w w) where
    mkSquare w = Rect (w,w)
instance OpDynMakeRectangle (Rect w h) where
    mkRectangle = Rect




newtype Sq w = Sq { unSq :: w }
instance RectangleConcept (Sq w) where
    type Width (Sq w) = w
    type Height (Sq w) = w
instance SquareConcept (Sq w)
instance Rectangle (Sq w) where
    getWidth = unSq
    getHeight = unSq
instance OpDynSquare (Sq w) where
    setBothWidthHeight w _ = Sq w
instance OpDynMakeSquare (Sq w) where
    mkSquare = Sq









sq :: Sq w
sq = undefined
rect :: Rect w h
rect = undefined
rect_ww :: Rect w w
rect_ww = undefined

type Is a = a -> a
isDynSquare :: DynSquare a => Is a
isDynRectangle__SameType :: DynRectangle__SameType a => Is a
isDynRectangle :: DynRectangle a => Is a
isOpDynMakeRectangle :: OpDynMakeRectangle a => Is a
isOpDynMakeSquare :: OpDynMakeSquare a => Is a

isDynSquare = id
isDynRectangle__SameType = id
isDynRectangle = id
isOpDynMakeSquare = id
isOpDynMakeRectangle = id


r = ( isDynRectangle rect
    , isDynRectangle__SameType rect_ww
    , isDynSquare sq
    , isOpDynMakeRectangle rect
    , isOpDynMakeSquare sq
    )

