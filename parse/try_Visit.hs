{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

--- 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}

import TH_Utils as SU
import Control.Monad.Reader
import Data.Map as M



{-
class (Visit m a, MonadReader t m) => VisitByTable t m a where
instance (Visit m a, MonadReader t m) => VisitByTable t m a where
class Monad m => Visit m a where
    visit :: a -> m a
-}

-- method
type M a m = a -> m a
-- default context
type DC m typ = C m typ -- Monad m => typ
type DCM a m = DC m (M a m)
type C m typ = MonadReader (Table m) m => typ
type C_ m = MonadReader (Table m) m
type CM a m = C m (M a m)
data Table m = Table { tableA :: TableA m, tableB :: TableB m }
data TableA m = TableA { caseA1 :: M A m, caseA2 :: M A m }
data A = A1 | A2 deriving Show
data TableB m = TableB { caseB1 :: M B m, caseB2 :: M B m, wildB :: M B m }
data B = B1 | B2 Int deriving Show

default_caseA1 :: DCM A m
default_caseA1 = return
default_caseA2 :: DCM A m
default_caseA2 = return
default_tableA :: DC m (TableA m)
default_tableA = TableA {caseA1 = default_caseA1, caseA2 = default_caseA2}
default_table :: DC m (Table m)
default_table = Table {tableA = default_tableA}

default_caseB1 :: DCM B m
default_caseB1 = return
default_caseB2 :: DCM B m
default_caseB2 (B2 a) = do
    a' <- visit a
    return $ B2 a'
default_tableB :: DC m (TableB m)
default_tableB = TableB {caseB1 = default_caseB1, caseB2 = default_caseB2}

update_caseA1_ :: C m ((A -> m A) -> TableA m -> TableA m)
update_caseA1_ f ta@TableA {caseA1 = ca1} = ta' where
    ca1' = ca1 >=> f
    ta' = ta {caseA1 = ca1'}
updateA_ :: C m ((A -> m A) -> TableA m -> TableA m)
updateA_ f ta@TableA {caseA1 = ca1, caseA2 = ca2} = ta' where
    -- ca1 = caseA1 ta
    ca1' = ca1 >=> f
    ca2' = ca2 >=> f
    ta' = ta {caseA1 = ca1', caseA2 = ca2'}
update_caseA1 :: C m ((A -> m A) -> Table m -> Table m)
update_caseA1 f t = t' where
    ta = tableA t
    ta' = update_caseA1_ f ta
    t' = t {tableA = ta'}

updateA :: C_ m => (A -> m A) -> Table m -> Table m
updateA f t = t' where
    ta = tableA t
    ta' = updateA_ f ta
    t' = t {tableA = ta'}

visitBy_ :: (t_~Table, m_~(->), m~RM t_ m_, t~t_ m, VisitByTable t m a)
    => a -> m a
visitBy_ = visit
visitBy :: (t_~Table, m_~(->), m~RM t_ m_, t~t_ m, VisitByTable t m a)
    => a -> Table m -> a
visitBy = unRM . visitBy_

table__A1toA2 :: C m (Table m)
table__A1toA2 = update_caseA1 (return . _flipA1_A2) default_table where
table__flipA1_A2 :: C m (Table m)
table__flipA1_A2 = updateA (return . _flipA1_A2) default_table where
_flipA1_A2 = f where
    f A1 = A2
    f A2 = A1
flipA1_A2 :: A -> A
flipA1_A2 a = visitBy a table__flipA1_A2
flipA1 :: A -> A
flipA1 a = visitBy a table__A1toA2



instance MonadReader (Table m) m => Visit m Int where
    visit = return

{-
SU.toInstance_ [t|Table|] "A" [t|A|] [('A1, 0), ('A2, 0)]
-}
instance MonadReader (Table m) m => Visit m A where
    visit x = let table = tableA in case x of
        A1 -> do
            caseA1_ <- asks $ caseA1 . table
            caseA1_ x
        A2 -> do
            caseA2_ <- asks $ caseA2 . table
            caseA2_ x

qB = fmap head [d|
    instance MonadReader (Table m) m => Visit m B where
        visit x = case x of
            B1 -> do
                _caseB1 <- get_case caseB1
                _caseB1 x
            B2 _ -> do
                _caseB2 <- get_case caseB2
                _caseB2 x
            _ -> do
                _wildB <- get_case wildB
                _wildB x
          where
            get_case caseBx = asks $ caseBx . tableB
    |]

data XXX v = YYY1 Int Bool v
qD = [d|
    default_caseYYY1 :: (Visit m Int, Visit m Bool) => XXX a -> m (XXX a)
    default_caseYYY1 (YYY1 a1 a2 v) = do
        a1' <- visit a1
        a2' <- visit a2
        return $ YYY1 a1' a2' v
    |]


qT = [d|
    data TableVT v m = TableVT {tableXXX :: TableXXX v m}
    data TableXXX v m = TableXXX {wildX, caseYYY1 :: XXX v -> m (XXX v)}
    default_TableXXX :: (Monad m, Visit m v) => TableXXX v m
    default_TableXXX = TableXXX
        {wildX = return, caseYYY1 = default_caseYYY1}
    default_caseYYY1 :: (Visit m Int, Visit m Bool) => XXX a -> m (XXX a)
    default_caseYYY1 (YYY1 a1 a2 v) = do
        a1' <- visit a1
        a2' <- visit a2
        return $ YYY1 a1' a2' v
    |]


{-

[SigD default_TableXXX (ForallT [PlainTV m,PlainTV v] [ClassP Monad [VarT m],ClassP Visit [VarT m,VarT v]] (AppT (AppT (ConT TableXXX) (VarT v)) (VarT m)))
,ValD (VarP default_TableXXX) (NormalB (RecConE TableXXX [(caseYYY1,VarE default_caseYYY1)])) []
]
-}
{-
[DataD [] TableVT [PlainTV v,PlainTV m]
    [RecC TableVT
        [(tableXXX
         ,NotStrict
         ,AppT (AppT (ConT TableXXX) (VarT v)) (VarT m)
         )
        ]
    ]
    []
,DataD [] TableXXX [PlainTV v,PlainTV m]
    [RecC TableXXX
        [(caseYYY1
         ,NotStrict
         ,AppT
            (AppT ArrowT (AppT (ConT Main.XXX) (VarT v)))
            (AppT (VarT m) (AppT (ConT Main.XXX) (VarT v)))
         )
        ]
    ]
    []
]
-}
{-

[SigD default_caseYYY1
    (ForallT [PlainTV m,PlainTV a]
        [ClassP Visit [VarT m,ConT Int]
        ,ClassP Visit [VarT m,ConT Bool]
        ]
        (AppT
            (AppT ArrowT (AppT (ConT XXX) (VarT a)))
            (AppT (VarT m) (AppT (ConT XXX) (VarT a)))
        )
    )
,FunD default_caseYYY1
    [Clause [ConP YYY1 [VarP a1,VarP a2,VarP v]]
        (NormalB (DoE
            [BindS (VarP a1') (AppE (VarE visit) (VarE a1))
            ,BindS (VarP a2') (AppE (VarE visit) (VarE a2))
            ,NoBindS (InfixE
                (Just (VarE return))
                (VarE $)
                (Just (AppE (AppE (AppE (ConE YYY1) (VarE a1')) (VarE a2')) (VarE v))))]
        ))
        []
    ]
]

-}
{-

[SigD default_caseXXX
    (ForallT [PlainTV m]
        [ClassP Visit [VarT m, ConT Int]
        ,ClassP Visit [VarT m,ConT Bool]
        ]
        (AppT (AppT ArrowT (ConT XXX)) (AppT (VarT m) (ConT XXX)))
    )
,FunD default_caseXXX [Clause
    [ConP Main.XXX [VarP a1,VarP a2]]
    (NormalB (DoE
        [BindS (Var P a1')
            (AppE (VarE TH_Utils.visit) (VarE a1))
        ,BindS (VarP a2')
            (AppE (VarE TH_Utils.visit) (VarE a2))
        ,NoBindS (InfixE
          (Just (VarE return))
          (VarE $)
          (Just (AppE (AppE (ConE Main.XXX) (VarE a1')) (VarE a2')))
         )
        ]
    ))
    []
  ]
]
-}
{-

InstanceD
    [ClassP
        MonadReader
        [AppT (ConT Main.Table) (VarT m_0),VarT m_0]
    ]
    (AppT (AppT (ConT Main.Visit) (VarT m_0)) (ConT Main.B))

    [FunD Main.visit [Clause [VarP x_1]
      (NormalB (CaseE (VarE x_1)
        [Match (ConP Main.B1 []) (NormalB (DoE [BindS (VarP _caseB1_3) (AppE (VarE get_case_2) (VarE Main.caseB1)),NoBindS (AppE (VarE _caseB1_3) (VarE x_1))])) []
        ,Match (ConP Main.B2 [WildP]) (NormalB (DoE [BindS (VarP _caseB2_4) (AppE (VarE get_case_2) (VarE Main.caseB2)),NoBindS (AppE (VarE _caseB2_4) (VarE x_1))])) []
        ,Match WildP (NormalB (DoE [BindS (VarP _wildB_5) (AppE (VarE get_case_2) (VarE Main.wildB)),NoBindS (AppE (VarE _wildB_5) (VarE x_1))])) []
        ]
      ))
      [FunD get_case_2 [Clause [VarP caseBx_5] (NormalB (InfixE (Just (VarE Control.Monad.Reader.Class.asks)) (VarE GHC.Base.$) (Just (InfixE (Just (VarE caseBx_5)) (VarE GHC.Base..) (Just (VarE Main.tableB)))))) []]
      ]
    ]]

-}
main = do
    -- Template Haskell error: Can't do `reify' in the IO monad
    -- printQ $ reify ''Visit
    -- reifyInstances ''Visit [VarT $ mkName "m", ConT ''Int]
    printLnQ qB
    printLnQ qD
    printLnQ qT
    {-
    let sig_funQ = do
        m <- newName "m"
        v <- newName "v"
        let t4 = return (VarT m, [m]
                , M.fromList [('YYY1, [typeInt, typeBool, type_v])]
                , M.fromList [(typeXXX, [v]), (typeInt, []), (typeBool, [])]
                )
            type_v = VarT v
            typeXXX = call AppT (ConT ''XXX) [type_v]
            typeInt = ConT ''Int
            typeBool = ConT ''Bool
        sig <- to_case_sig "YYY1" 'YYY1 (return typeXXX) t4
        fun <- to_case_fun "YYY1" 'YYY1 (return typeXXX) t4
        return [sig, fun]
    pprintLnQ sig_funQ

    let dataQ = do
        m <- newName "m"
        v <- newName "v"
        let t2 = return (m
                , M.fromList [(typeXXX, [v]), (typeInt, []), (typeBool, [])]
                )
            type_v = VarT v
            typeXXX = call AppT (ConT ''XXX) [type_v]
            typeInt = ConT ''Int
            typeBool = ConT ''Bool

        def_Table__Type2Case "XXX" typeXXX ["XXX1"] t2
    pprintLnQ dataQ
    -}


_test1 = typeAssertMonadReader (undefined :: RM Table (->) a)
_test2 = typeAssertMonadReader (undefined :: RT Table ReaderT IO a)




-- fmap return $ SU.toInstance_ [t|Table|] "A" [t|A|] [('A1, 0), ('A2, 0)]
-- fmap return $ SU.toInstance__ty_con_name [t|Table|] "A" [t|A|] ''A
-- fmap return $ SU.toInstance [t|Table|] "A" [t|A|]


