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

class Value t where
  value :: t -> Bool

instance Value HExp where
  value e = case e of
    HFunAbs _ _ _ -> True
    HNum _ -> True
    HTyAbs _ _ -> True
    _ -> False

instance Value MExp where
  value e = case e of
    MFunAbs _ _ _ -> True
    MNum _ -> True
    MTyAbs _ _ -> True
    _ -> False

instance Value SExp where
  value = undefined

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
  HAdd x y | not $ value x -> do
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
  HM t (MH t' e) | t == t' -> return e
  HM Lump (MS Lump e) | value e -> return $ HS Lump e
  HM Nat (MNum n) -> return $ HNum n
  HM (Fun p r) f @ (MFunAbs v t b) | p == t -> return $ HFunAbs v p (HM r (MFunApp f (MH p (HVar v))))
  HM (Forall v t) (MTyAbs v' e) | v == v'-> return $ HTyAbs v (HM t e)
  HM f @ (Forall v t) (MS (Forall v' t') e) | v == v' && t == t' && value e -> return $ HS f e
  HS t (SH t' e) | t == t' -> return e
  HS Nat (SNum n) -> return $ HNum n
  HS Nat e | value e -> return $ HWrong Nat "Not a number"
  HS (Label t _) e | value e -> return $ HWrong t "Parametricity violated"
  HS (Fun p r) f @ (SFunAbs v b) -> return $ HFunAbs v (unlabel p) (HS r (SFunApp f (SH p (HVar v))))
  HS t @ (Fun _ _) e | value e -> return $ HWrong (unlabel t) "Not a function"
  HSub (HNum x) (HNum y) -> return . HNum $ max 0 $ x - y
  HSub x y | not $ value x -> do
    x' <- reduceH x
    return $ HSub x' y
  HSub x y -> do
    y' <- reduceH y
    return $ HSub x y'
  HTyApp (HTyAbs v b) t -> return $ substTyExp t v b
  HTyApp (HS (Forall v t) e) t' | value e -> do
    label <- get
    put (label + 1)
    return $ HS (substTy (Label t' label) v t) e
  HTyApp x y -> do
    x' <- reduceH x
    return $ HTyApp x' y
  HWrong _ s -> fail s
  _ -> irreducible
