module Reduce (
  reduceH)
  where

import Syntax

unlabel :: SType -> SType
unlabel (Label t) = t
unlabel (Fun x y) = Fun (unlabel x) (unlabel y)
unlabel (Forall x y) = Forall x (unlabel y)
unlabel x = x

substExpH :: HExp -> HExp -> HExp -> HExp
substExpH new old body = body

--reduceAllH :: [TyDef] -> HExp -> Maybe HExp
--reduceAllH = 

--class Context a where
--  context :: [TyDef] -> 

valueH (HCon _ _) = True
valueH (HFunAbs _ _ _) = True
valueH (HNum _) = True
valueH (HTyAbs _ _) = True
valueH _ = False

contextH :: [TyDef] -> HExp -> Maybe HExp
contextH d x @ (HFunApp (HFunAbs _ _ _) _) = reduceH d x
contextH d (HFunApp x y) = do
  x' <- contextH d x
  reduceH d $ HFunApp x' y
contextH _ _ = Nothing

reduceH :: [TyDef] -> HExp -> Maybe HExp
reduceH _ (HAdd (HNum x) (HNum y)) = Just . HNum $ x + y
reduceH _ f @ (HFix (HFunAbs v _ b)) = Just $ substType f (HVar v) b
reduceH _ (HFunApp (HFunAbs v t b) a) = Just $ substExpH a (HVar v) b
reduceH d (HField fn (HCon cn f)) = 
reduceH _ _ = Nothing




{-reduceH _ (HAdd (HNum x) (HNum y)) = do
  return . HNum $ x + y
reduce d (HAdd x @ (HNum _) y) = do
  return $ HAdd x $ reduceH d y
reduceH d (HAdd x y @ (HNum _)) = do
  return (HAdd (reduceH d x) y)-}
