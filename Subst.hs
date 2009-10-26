module Subst (substExpH, substExpM, substExpS, substTyExpH, substTyExpM, substTyTy) where

import Syntax

maph :: (HExp -> Bool) -> (HExp -> HExp) -> HExp -> HExp
maph p f e = case e of
  w @ (HAdd x y) -> if p w then f w else HAdd (maph x) (maph y)

substExpH new old exp = 

substExpH :: HExp -> EVar -> HExp -> HExp
substExpH new old exp = case exp of
  HAdd x y -> HAdd (subst x) (subst y)
  HFix x -> HFix $ subst x
  f @ (HFunAbs v _ _) | old == v -> f
  HFunAbs v t b -> HFunAbs v t $ subst b
  HFunApp x y -> HFunApp (subst x) (subst y)
  HIf0 x y z -> HIf0 (subst x) (subst y) (subst z)
  HM t e -> HM t $ substHM new old e
  x @ (HNum _) -> x
  HSub x y -> HSub (subst x) (subst y)
  HTyAbs v b -> HTyAbs v $ subst b
  HTyApp e t -> HTyApp (subst e) t
  HVar x | old == x -> new
  x @ (HVar _) -> x
  x @ (HWrong _ _) -> x
  where subst = substExpH new old

substHM :: HExp -> EVar -> MExp -> MExp
substHM new old exp = case exp of
  MAdd x y -> MAdd (subst x) (subst y)
  MFix x -> MFix $ subst x
  MFunAbs v t b -> MFunAbs v t $ subst b
  MFunApp x y -> MFunApp (subst x) (subst y)
  MIf0 x y z -> MIf0 (subst x) (subst y) (subst z)
  MH t e -> MH t $ substExpH new old e
  x @ (MNum _) -> x
  MSub x y -> MSub (subst x) (subst y)
  MTyAbs v b -> MTyAbs v $ subst b
  MTyApp e t -> MTyApp (subst e) t
  x @ (MVar _) -> x
  x @ (MWrong _ _) -> x
  where
    subst = substHM new old

substHS = undefined
    
substExpM :: MExp -> EVar -> MExp -> MExp
substExpM new old exp = case exp of
  MAdd x y -> MAdd (subst x) (subst y)
  MFix x -> MFix $ subst x
  f @ (MFunAbs v _ _) | old == v -> f
  MFunAbs v t b -> MFunAbs v t $ subst b
  MFunApp x y -> MFunApp (subst x) (subst y)
  MIf0 x y z -> MIf0 (subst x) (subst y) (subst z)
  MH t e -> MH t $ substMH new old e
  x @ (MNum _) -> x
  MSub x y -> MSub (subst x) (subst y)
  MTyAbs v b -> MTyAbs v $ subst b
  MTyApp e t -> MTyApp (subst e) t
  MVar x | old == x -> new
  x @ (MVar _) -> x
  x @ (MWrong _ _) -> x
  where subst = substExpM new old

substMH :: MExp -> EVar -> HExp -> HExp
substMH new old exp = case exp of
  HAdd x y -> HAdd (subst x) (subst y)
  HFix x -> HFix $ subst x
  HFunAbs v t b -> HFunAbs v t $ subst b
  HFunApp x y -> HFunApp (subst x) (subst y)
  HIf0 x y z -> HIf0 (subst x) (subst y) (subst z)
  HM t e -> HM t $ substExpM new old e
  x @ (HNum _) -> x
  HSub x y -> HSub (subst x) (subst y)
  HTyAbs v b -> HTyAbs v $ subst b
  HTyApp e t -> HTyApp (subst e) t
  x @ (HVar _) -> x
  x @ (HWrong _ _) -> x
  where
    subst = substMH new old

substMS = undefined

substExpS :: SExp -> EVar -> SExp -> SExp
substExpS new old exp = case exp of
  SAdd x y -> SAdd (subst x) (subst y)
  f @ (SFunAbs v _) | old == v -> f
  SFunAbs v b -> SFunAbs v $ subst b
  SFunApp x y -> SFunApp (subst x) (subst y)
  SH t e -> SH t $ substSH new old e
  SIf0 x y z -> SIf0 (subst x) (subst y) (subst z)
  SM t e -> SM t $ substSM new old e
  x @ (SNum _) -> x
  SSub x y -> SSub (subst x) (subst y)
  SVar x | old == x -> new
  x @ (SVar _) -> x
  x @ (SWrong _) -> x
  where subst = substExpS new old

substSH = undefined

substSM = undefined
    
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

substTyExpM :: SType -> TVar -> MExp -> MExp
substTyExpM new old exp = go exp where
  substE = substTyExpM new old
  substT = substTyTy new old
  go (MAdd x y) = MAdd (substE x) (substE y)
  go (MFix x) = MFix $ substE x
  go (MFunAbs v t b) = MFunAbs v (substT t) b
  go (MFunApp x y) = MFunApp (substE x) (substE y)
  go (MIf0 x y z) = MIf0 (substE x) (substE y) (substE z)
  go x @ (MNum _) = x
  go (MSub x y) = MSub (substE x) (substE y)
  go x @ (MTyAbs v _) | old == v = x
  go (MTyAbs v b) = MTyAbs v (substE b)
  go (MTyApp a t) = MTyApp a (substT t)
  go x @ (MVar _) = x
  go x @ (MWrong _ _) = x

substTyTy :: SType -> TVar -> SType -> SType
--substTyTy new old (Ext n t) = Ext n $ map (substTyTy new old) t
substTyTy new old forall @ (Forall var body) | old /= var = Forall var (substTyTy new old body)
                                             | otherwise = forall
substTyTy new old (Fun param body) = Fun (substTyTy new old param) (substTyTy new old body)
substTyTy _ _ x @ (Label _ _) = x
substTyTy _ _ Lump = Lump
substTyTy _ _ Nat = Nat
substTyTy new old (TyVar var) | old == var = new
substTyTy _ _ x @ (TyVar _) = x
