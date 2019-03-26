{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ImplicitParams #-}


module ADT.ISemiringBy
where
import ADT.IPair
import Data.Semigroup hiding (Sum, Product)
import Data.Proxy
import GHC.Types (type (~~))
import Data.Kind (type (*))
import Seed.ProxyOps (last1P)
import Seed.By
import Seed.Test
import ADT.SemigroupBy

class (ISemigroupBy by a, ISemigroupBy (SemiringBy2SumBy by a) a)
    => ISemiringBy by a where
    type SemiringBy2SumBy by a
    -- a *? (b +? c) === a *? b +? a *? c
    -- (b +? c) *? a === b *? a +? c *? a
    -- i.e. G = (*,+,{n in Integer | n > 3})
    addBy :: proxy by -> a -> a -> a
    addBy p = mulBy . addBy2mulBy
    (+?) :: (?by :: proxy by) => a -> a -> a
    (+?) = addBy ?by

    {-
        exist mul_err ==>> mul_err = mul_err + mul_err
                        via. mul_err * (mul_err+mul_err)
        ?? sum_err * sum_err == sum_err ?? No, 3)
        ?? sum_id === mul_err ?? No, 1) 4)
        ?? sum_err === mul_err ?? No, 2) 3)
        ?? mul_err === sum_id or sum_err ?? No, 5)
        if exist sum_id, sum_err, mul_err, mul_id:
            ?? mul_err === sum_id or sum_err ??  No. 8)
    -}

    {-
        example:
        1) G = {sum_id=mul_id, mul_err=sum_err}
            * = &&
            + = &&
            sum_id = True
            mul_err = False
        2) G = {sum_err=mul_id, mul_err=sum_id}
            * = &&
            + = ||
            mul_err = False
            sum_err = True
        3) G = {sum_err, mul_err=sum_id} -- no mul_id
            * = const2 mul_err
            + = ||
            sum_err = True
            mul_err = False
        4) G = {sum_id, mul_err=sum_err} -- no mul_id
            * = const2 mul_err
            + = ||
            sum_id = False
            mul_err = True
        5) G = {mul_err, sum_err, sum_id} -- no mul_id
            * = const2 mul_err
            + = ||
            sum_err = True
            sum_id = False
            mul_err = Unknown
            &&:
                T* = T -- all T
                F.* = F -- any F
                U[UT]* = U
            ||:
                F* = F -- all F
                T.* = -- any T
                U[UF] = U


        6) G = {TF, TU, UU, UF}
            -- T_ -   known T - |- [P]
            -- U_ - unknown T - can not |- [P]
            -- _U - unknown F - can not |- [not P]
            -- _F -   known F - |- [not P]
            -- |- [P and Q] <==> |- [P] and |- [Q]
            -- |- [not (P and Q)] <==> |- [not P or not Q]
            --                    <==> |- [not P] or |- [not Q]
            -- (|- [P], |- [not P]) && (|- [Q], |- [not Q])
            --      == (|- [P and Q], |- [not (P and Q)])
            --      == (p1 && q1, p2 || q2)
            * = &&
            + = ||
            g = [TU][UF]
                for [TU]:
                    mul_id = T, mul_err = U
                    sum_id = U, sum_err = T
                    T && T = T
                    _ && _ = U
                    U || U = U
                    _ || _ = T
                for [UF]:
                    mul_id = F, mul_err = U
                    sum_id = U, sum_err = F
                    F && F = F
                    _ && _ = U
                    U || U = U
                    _ || _ = F
            ab && cd = (a && c)(b || d)
            ab || cd = (a || c)(b && d)
            ab && ab = ab
            ab || ab = ab
            mul_id  = TU
            mul_err = UF
            sum_id  = UF
            sum_err = TU
            &&: TU < [UU|TF] < UF
            UU && TF = UF
            ||: UF < [UU|TF] < TU
            UU || TF = TU
        7) G = {T, F, X, U}
                -- X - Error -- raise Exception
                -- T - True  -- return T
                -- F - False -- return F
                -- U - Unknown Yet -- noreturn
                U && X = X
                U || X = U
                mul_id = T
                mul_err = F
                sum_err = T
                sum_id = F
                &&: T < U < X < F
                ||: F < X < U < T
                U && (T || _) = U
                U && (_ || T) = U
                U && T || U && xT = U || xT = U
                U && a@(_ || _) = a
                U && xT1 || U && xT2 = xT1 || xT2 = a
                [not (a || b) = not a && not b] ==>>
                    not T = not (U || T) = not U && not T = F
                    not U = not (U || xT) = not U && not xT = not U && xF = X
                    let not U = X, not X = U
                    xf = not ut = not (X || ut)
                    = not X && not ut = not X && not ut = U && xf = xf


        8) G = {mul_err, sum_err, sum_id, mul_id}
            -- calc by find_semirings.py
            -- commutative four semiring ==>> only one
            {mul_err, mul_id} + {mul_err, mul_id} == ??
            {sum_err, sum_id} * {sum_err, sum_id} == ??
            a + a = a
            a * a = a
            mul_id + mul_err = sum_err
            sum_id * sum_err = mul_err

            sum_id = F
            mul_id = T
            mul_err = X
            sum_err = U
            &&: T < [F|U] < X
            ||: F < [T|X] < U
            F && U == X
            X || T == U

    -}

infixr 5 +?
addBy2mulBy :: proxy by -> a -> Proxy (SemiringBy2SumBy by a)
addBy2mulBy = Proxy










class (Semigroup a, ISemiringBy () a) => Semiring a where
instance (Semigroup a, ISemiringBy () a) => Semiring a where



instance ISemiringBy by a => ISemiringBy (ByDual by) a where
    type SemiringBy2SumBy (ByDual by) a = ByDual (SemiringBy2SumBy by a)
instance Num a => ISemiringBy ByProduct a where
    type SemiringBy2SumBy ByProduct a = BySum
instance TestBy by Bool => ISemiringBy (ByAny by) Bool where
    type SemiringBy2SumBy (ByAny by) Bool = (ByAll by)
instance TestBy by Bool => ISemiringBy (ByAll by) Bool where
    type SemiringBy2SumBy (ByAll by) Bool = (ByAny by)
instance ISemiringBy ByFirst (Maybe a) where
    -- both ByFirst and ByLast
    type SemiringBy2SumBy ByFirst (Maybe a) = ByLast a

instance Ord a => ISemiringBy ByMin a where
    type SemiringBy2SumBy ByMin a = ByMax





instance (IPair a, ISemiringBy by1 (Fst a), ISemiringBy by2 (Snd a))
    => ISemiringBy (by1 :*: by2) a where
    type SemiringBy2SumBy (by1 :*: by2) a =
        SemiringBy2SumBy by1 (Fst a) :*: SemiringBy2SumBy by2 (Snd a)









