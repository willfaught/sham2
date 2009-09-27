module Subst (
  substExpH,
  substTyExpH,
  substTyTy)
  where

import Syntax

substExpH :: HExp -> EVar -> HExp -> HExp
substExpH new old exp = go exp where
  subst = substExpH new old
  go (HAdd x y) = HAdd (subst x) (subst y)
  go (HFix x) = HFix $ subst x
  go f @ (HFunAbs v _ b) | old == v = f
  go (HFunAbs v t body) = HFunAbs v t $ substExpH new old body
  go (HFunApp x y) = HFunApp (subst x) (subst y)
  go (HIf0 x y z) = HIf0 (subst x) (subst y) (subst z)
  go x @ (HNum _) = x
  go (HSub x y) = HSub (subst x) (subst y)
  go (HTyAbs v b) = subst b
  go (HTyApp a t) = HTyApp (subst a) t
  go (HVar x) | old == x = new
  go x @ (HVar _) = x
  go x @ (HWrong _ _) = x

substTyExpH :: SType -> TVar -> HExp -> HExp
substTyExpH new old exp = go exp where
  substE = substTyExpH new old
  substT = substTyTy new old
  go (HAdd x y) = HAdd (substE x) (substE y)
  go (HFix x) = HFix $ substE x
  go (HFunAbs v t b) = HFunAbs v (substT t) b
  go (HFunApp x y) = HFunApp (substE x) (substE y)
  go (HIf0 x y z) = HIf0 (substE x) (substE y) (substE z)
  go x @ (HNum _) = x
  go (HSub x y) = HSub (substE x) (substE y)
  go x @ (HTyAbs v _) | old == v = x
  go (HTyAbs v b) = HTyAbs v (substE b)
  go (HTyApp a t) = HTyApp a (substT t)
  go x @ (HVar _) = x
  go x @ (HWrong _ _) = x

substTyTy :: SType -> TVar -> SType -> SType
--substTyTy new old (Ext n t) = Ext n $ map (substTyTy new old) t
substTyTy new old forall @ (Forall var body) | old /= var = Forall var (substTyTy new old body)
                                             | otherwise = forall
substTyTy new old (Fun param body) = Fun (substTyTy new old param) (substTyTy new old body)
substTyTy _ _ x @ (Label _) = x
substTyTy _ _ Lump = Lump
substTyTy _ _ Nat = Nat
substTyTy new old (TyVar var) | old == var = new
substTyTy _ _ x @ (TyVar _) = x
