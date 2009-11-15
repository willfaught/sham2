module Reduce (reduce, reduceFull) where

import Data.Either
import Subst
import Syntax

unlabel :: SType -> SType
unlabel (Label t _) = t
unlabel (Fun x y) = Fun (unlabel x) (unlabel y)
unlabel (Forall x y) = Forall x (unlabel y)
unlabel x = x

-- Reduce

class (Show t, Eq t) => Reduce t where
  reduce :: t -> Reduction t

instance Reduce HExp where
  reduce = reduceH

instance Reduce MExp where
  reduce = reduceM

-- ReduceFull

class Reduce t => ReduceFull t where
  reduceFull :: t -> Reduction t

instance ReduceFull HExp where
  reduceFull = reduceFullH

instance ReduceFull MExp where
  reduceFull = reduceFullM

-- Values

valueH :: HExp -> Bool
valueH (HFunAbs _ _ _) = True
valueH (HNum _) = True
valueH (HTyAbs _ _) = True
valueH _ = False

valueM :: MExp -> Bool
valueM (MFunAbs _ _ _) = True
valueM (MNum _) = True
valueM (MTyAbs _ _) = True
valueM _ = False

-- Full reductions

reduceFullH :: HExp -> Reduction HExp
reduceFullH x = case reduce x of
  Reduced x' -> reduceFullH x'
  Unreduced -> Unreduced
  Error s -> Error s

reduceFullM :: MExp -> Reduction MExp
reduceFullM x = case reduce x of
  Reduced x' -> reduceFullM x'
  Unreduced -> Unreduced
  Error s -> Error s

-- Reductions

-- Haskell

data Reduction e = Reduced e | Unreduced | Error String deriving (Show, Eq)

instance Monad Reduction where
  x >>= f = case x of
    Reduced e -> f e
    Unreduced -> Unreduced
    Error s -> Error s
  return = Reduced
  fail = Error

reduceH :: HExp -> Reduction HExp
reduceH exp = case exp of
  HAdd (HNum x) (HNum y) -> return . HNum $ x + y
  HAdd x y | not $ valueH x -> do
    x' <- reduceH x
    return $ HAdd x' y
  HAdd x y -> do
    y' <- reduceH y
    return $ HAdd x y'
  HFix (HFunAbs v _ b) -> return $ substExp exp v b
  HFix x -> do
    x' <- reduceH x
    return $ HFix x'
  HFunApp (HFunAbs v t b) a -> return $ substExp a v b
  HFunApp x y -> do
    x' <- reduceH x
    return $ HFunApp x' y
  HIf0 (HNum n) t f -> return $ if n == 0 then t else f
  HIf0 x t f -> do
    x' <- reduceH x
    return $ HIf0 x' t f
  HM _ (MH _ e) -> return e
  HM _ (MNum n) -> return $ HNum n
  HM (Fun p r) f @ (MFunAbs v t b) -> return $ HFunAbs v p (HM r (MFunApp f (MH p (HVar v))))
  HM (Forall v t) (MTyAbs w e) -> return $ HTyAbs v (HM t e)
  HS _ (SH _ e) -> return e
  HS Nat (SNum n) -> return $ HNum n
  HS Nat _ -> return $ HWrong Nat "Not a number"
  HS (Label t _) _ -> return $ HWrong t "Parametricity violated"
  HS (Fun p r) f @ (SFunAbs v b) -> return $ HFunAbs v (unlabel p) (HS r (SFunApp f (SH p (HVar v))))
  HS t @ (Fun _ _) _ -> return $ HWrong (unlabel t) "Not a function"
  HSub (HNum x) (HNum y) -> return . HNum $ x + y
  HSub x y | not $ valueH x -> do
    x' <- reduceH x
    return $ HSub x' y
  HSub x y -> do
    y' <- reduceH y
    return $ HSub x y'
  HTyApp (HTyAbs v b) t -> return $ substTyExp t v b
  HTyApp (HS (Forall v t) e) u -> return $ HS (substTy (Label u 0) v t) e
  HTyApp x y -> do
    x' <- reduceH x
    return $ HTyApp x' y
  HWrong _ s -> fail s
  _ -> Unreduced

-- ML

reduceM = undefined
