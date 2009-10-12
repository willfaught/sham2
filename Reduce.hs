module Reduce (reduceH) where

import Subst
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

reduceFullH :: HExp -> HExp
reduceFullH x = case reduceH x of
  Just x' -> reduceFullH x'
  Nothing -> x

reduceH :: HExp -> Maybe HExp
reduceH (HAdd (HNum x) (HNum y)) = return . HNum $ x + y
reduceH (HAdd x y) | not $ valueH x = do
  x' <- reduceH x
  return $ HAdd x' y
reduceH (HAdd x y) = do
  y' <- reduceH y
  return $ HAdd x y'
reduceH f @ (HFix (HFunAbs v _ b)) = return $ substExpH f v b
reduceH (HFix x) = do
  x' <- reduceH x
  return $ HFix x'
reduceH (HFunApp (HFunAbs v t b) a) = return $ substExpH a v b
reduceH (HFunApp x y) = do
  x' <- reduceH x
  return $ HFunApp x' y
--reduceH (HField fn (HCon cn f)) = 
reduceH (HIf0 (HNum 0) t _) = Just t
reduceH (HIf0 (HNum _) _ f) = Just f
reduceH (HIf0 x t f) = do
  x' <- reduceH x
  return $ HIf0 x' t f
reduceH (HSub (HNum x) (HNum y)) = Just . HNum $ x + y
reduceH (HSub x y) | not $ valueH x = do
  x' <- reduceH x
  return $ HSub x' y
reduceH (HSub x y) = do
  y' <- reduceH y
  return $ HSub x y'
reduceH (HTyApp (HTyAbs v b) t) = return $ substTyExpH t v b
reduceH (HTyApp x y) = do
  x' <- reduceH x
  return $ HTyApp x' y
reduceH (HWrong _ s) = error s
reduceH _ = Nothing
