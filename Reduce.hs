module Reduce (
  reduceH)
  where

import Syntax

unlabel :: SType -> SType
unlabel (Label t) = t
unlabel (Fun x y) = Fun (unlabel x) (unlabel y)
unlabel (Forall x y) = Forall x (unlabel y)
unlabel x = x

valueH :: HExp -> Bool
--valueH (HCon _ _) = True
valueH (HFunAbs _ _ _) = True
valueH (HNum _) = True
valueH (HTyAbs _ _) = True
valueH _ = False

{-contextH :: HExp -> Maybe HExp
contextH x @ (HFunApp (HFunAbs _ _ _) _) = reduceH d x
contextH (HFunApp x y) = do
  x' <- contextH d x
  reduceH $ HFunApp x' y
contextH _ = Nothing-}

reduceH :: HExp -> Maybe HExp
reduceH (HAdd (HNum x) (HNum y)) = Just . HNum $ x + y
reduceH f @ (HFix (HFunAbs v _ b)) = Just $ substType f (HVar v) b
reduceH (HFunApp (HFunAbs v t b) a) = Just $ substExpExpH a (HVar v) b
--reduceH (HField fn (HCon cn f)) = 
reduceH (HIf0 (HNum 0) t _) = t
reduceH (HIf0 (HNum _) _ f) = f
reduceH (HSub (HNum x) (HNum y)) = Just . HNum $ x + y
reduceH (HTyApp (HTyAbs v b) t) = substTyExpH t v b
reduceH (HWrong _ s) = error s
reduceH _ = Nothing




{-reduceH _ (HAdd (HNum x) (HNum y)) = do
  return . HNum $ x + y
reduce d (HAdd x @ (HNum _) y) = do
  return $ HAdd x $ reduceH d y
reduceH d (HAdd x y @ (HNum _)) = do
  return (HAdd (reduceH d x) y)-}
