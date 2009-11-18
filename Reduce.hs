module Reduce (reduce, reduceFull) where

import Control.Monad.State
import Data.Either
import Subst
import Syntax

unlabel :: SType -> SType
unlabel t = case t of
  Label u _ -> u
  Fun p r -> Fun (unlabel p) (unlabel r)
  Forall v b -> Forall v (unlabel b)
  _ -> t

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

-- ReduceFull

class Reduce t => ReduceFull t where
  reduceFull :: t -> Either String t

instance ReduceFull HExp where
  reduceFull e = case reduce e of
    Just e' -> case e' of
      Left s -> Left s
      Right e -> reduceFull e
    Nothing -> Right e

-- Reduce

class (Show t, Eq t) => Reduce t where
  reduce :: t -> Maybe (Either String t)

instance Reduce HExp where
  reduce e = let Reduction r = evalStateT (reduceH e) 1 in r

-- Reduction

newtype Reduction e = Reduction (Maybe (Either String e)) deriving (Show, Eq)

instance Monad Reduction where
  Reduction x >>= f = case x of
    Just x' -> case x' of
      Left s -> Reduction . Just . Left $ s
      Right e -> f e
    Nothing -> Reduction Nothing
  return = Reduction . Just . Right
  fail = Reduction . Just . Left

irreducible :: StateT Int Reduction e
irreducible = StateT $ \ s -> Reduction Nothing

-- Haskell

reduceH :: HExp -> StateT Int Reduction HExp
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
  _ -> irreducible
