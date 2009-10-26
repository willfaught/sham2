module Subst (substTy, substTyExp, substExp) where

import Syntax

-- Types in types.

substTy :: SType -> TVar -> SType -> SType
substTy new old forall @ (Forall var body) | old /= var = Forall var (substTy new old body)
                                             | otherwise = forall
substTy new old (Fun param body) = Fun (substTy new old param) (substTy new old body)
substTy _ _ x @ (Label _ _) = x
substTy _ _ Lump = Lump
substTy _ _ Nat = Nat
substTy new old (TyVar var) | old == var = new
substTy _ _ x @ (TyVar _) = x

-- Types in expressions.

class SubstTyExp a where
  substTyExp :: SType -> TVar -> a -> a

instance SubstTyExp HExp where
  substTyExp = substTyExpH

substTyExpH :: SType -> TVar -> HExp -> HExp
substTyExpH new old = emap f where
  substE = substTyExpH new old
  substT = substTy new old
  f (HFunAbs v t b) = HFunAbs v (substT t) (substE b)
  f (HM t e) = HM (substT t) (substTyExpHM new old e)
  f (HS t e) = HS (substT t) (substTyExpHS new old e)
  f x @ (HTyAbs v e) | v == old = x | otherwise = HTyAbs v (substE e)
  f (HTyApp e t) = HTyApp e (substT t)
  f x = x

substTyExpHM :: SType -> TVar -> MExp -> MExp
substTyExpHM new old = emap f where
  f (MH t e) = MH t $ substTyExp new old e
  f x = x

substTyExpHS :: SType -> TVar -> SExp -> SExp
substTyExpHS new old = emap f where
  f (SH t e) = SH t $ substTyExp new old e
  f x = x

instance SubstTyExp MExp where
  substTyExp = substTyExpM

substTyExpM :: SType -> TVar -> MExp -> MExp
substTyExpM new old = emap f where
  substE = substTyExpM new old
  substT = substTy new old
  f (MFunAbs v t b) = MFunAbs v (substT t) (substE b)
  f (MH t e) = MH (substT t) (substTyExpMH new old e)
  f (MS t e) = MS (substT t) (substTyExpMS new old e)
  f x @ (MTyAbs v e) | v == old = x | otherwise = MTyAbs v (substE e)
  f (MTyApp e t) = MTyApp e (substT t)
  f x = x

substTyExpMH :: SType -> TVar -> HExp -> HExp
substTyExpMH new old = emap f where
  f (HM t e) = HM t $ substTyExp new old e
  f x = x

substTyExpMS :: SType -> TVar -> SExp -> SExp
substTyExpMS new old = emap f where
  f (SM t e) = SM t $ substTyExp new old e
  f x = x

-- Expressions in expressions.

class SubstExp a where
  substExp :: a -> EVar -> a -> a

instance SubstExp HExp where
  substExp = substExpH

substExpH :: HExp -> EVar -> HExp -> HExp
substExpH new old = emap f where
  f (HVar v) | v == old = new
  f x @ (HFunAbs v t b) | v == old = x
  f (HM t e) = HM t $ substExpHM new old e
  f (HS t e) = HS t $ substExpHS new old e
  f x = x

substExpHM :: HExp -> EVar -> MExp -> MExp
substExpHM new old = emap f where
  f (MH t e) = MH t $ substExp new old e
  f x = x
  
substExpHS :: HExp -> EVar -> SExp -> SExp
substExpHS new old = emap f where
  f (SH t e) = SH t $ substExp new old e
  f x = x

instance SubstExp MExp where
  substExp = substExpM

substExpM :: MExp -> EVar -> MExp -> MExp
substExpM new old = emap f where
  f (MVar v) | v == old = new
  f x @ (MFunAbs v t b) | v == old = x
  f (MH t e) = MH t $ substExpMH new old e
  f (MS t e) = MS t $ substExpMS new old e
  f x = x

substExpMH :: MExp -> EVar -> HExp -> HExp
substExpMH new old = emap f where
  f (HM t e) = HM t $ substExp new old e
  f x = x
  
substExpMS :: MExp -> EVar -> SExp -> SExp
substExpMS new old = emap f where
  f (SM t e) = SM t $ substExp new old e
  f x = x

instance SubstExp SExp where
  substExp = substExpS

substExpS :: SExp -> EVar -> SExp -> SExp
substExpS new old = emap f where
  f (SVar v) | v == old = new
  f x @ (SFunAbs v b) | v == old = x
  f (SH t e) = SH t $ substExpSH new old e
  f (SM t e) = SM t $ substExpSM new old e
  f x = x

substExpSH :: SExp -> EVar -> HExp -> HExp
substExpSH new old = emap f where
  f (HS t e) = HS t $ substExp new old e
  f x = x
  
substExpSM :: SExp -> EVar -> MExp -> MExp
substExpSM new old = emap f where
  f (MS t e) = MS t $ substExp new old e
  f x = x
