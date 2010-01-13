module Substitute (substTy, substTyExp, substExp) where

import Syntax

-- Types in types.

substTy :: SType -> TVar -> SType -> SType
substTy new old t = case t of
  Forall p b | p == old -> t
             | otherwise -> Forall p (subst b)
  Fun p r -> Fun (subst p) (subst r)
  TyVar v | v == old -> new
          | otherwise -> t
  _ -> t
  where subst = substTy new old

-- Types in expressions.

class SubstTyExp a where
  substTyExp :: SType -> TVar -> a -> a

instance SubstTyExp HExp where
  substTyExp new old exp = case exp of
    HFunAbs v t b -> HFunAbs v (substT t) (substE b)
    HM t e -> HM (substT t) (substTyExpHM new old e)
    HS t e -> HS (substT t) (substTyExpHS new old e)
    HTyAbs v e | v == old -> exp | otherwise -> HTyAbs v (substE e)
    HTyApp e t -> HTyApp e (substT t)
    _ -> emap (substTyExp new old) exp
    where substE = substTyExp new old
          substT = substTy new old

substTyExpHM :: SType -> TVar -> MExp -> MExp
substTyExpHM new old exp = case exp of
  MH t e -> MH t $ substTyExp new old e
  _ -> emap (substTyExpHM new old) exp

substTyExpHS :: SType -> TVar -> SExp -> SExp
substTyExpHS new old exp = case exp of
  SH t e -> SH t $ substTyExp new old e
  _ -> emap (substTyExpHS new old) exp

instance SubstTyExp MExp where
  substTyExp new old exp = case exp of
    MFunAbs v t b -> MFunAbs v (substT t) (substE b)
    MH t e -> MH (substT t) (substTyExpMH new old e)
    MS t e -> MS (substT t) (substTyExpMS new old e)
    MTyAbs v e | v == old -> exp | otherwise -> MTyAbs v (substE e)
    MTyApp e t -> MTyApp e (substT t)
    _ -> emap (substTyExp new old) exp
    where substE = substTyExp new old
          substT = substTy new old

substTyExpMH :: SType -> TVar -> HExp -> HExp
substTyExpMH new old exp = case exp of
  HM t e -> HM t $ substTyExp new old e
  _ -> emap (substTyExpMH new old) exp

substTyExpMS :: SType -> TVar -> SExp -> SExp
substTyExpMS new old exp = case exp of
  SM t e -> SM t $ substTyExp new old e
  _ -> emap (substTyExpMS new old) exp

-- Expressions in expressions.

class SubstExp a where
  substExp :: a -> EVar -> a -> a

instance SubstExp HExp where
  substExp new old exp = case exp of
    HVar v | v == old -> new
    HFunAbs v _ _ | v == old -> exp
    HFunAbs v t b -> HFunAbs v t (subst b)
    HM t e -> HM t $ substExpHM new old e
    HS t e -> HS t $ substExpHS new old e
    x -> emap subst x
    where subst = substExp new old

substExpHM :: HExp -> EVar -> MExp -> MExp
substExpHM new old exp = case exp of
  MH t e -> MH t $ substExp new old e
  x -> emap (substExpHM new old) x

substExpHS :: HExp -> EVar -> SExp -> SExp
substExpHS new old exp = case exp of
  SH t e -> SH t $ substExp new old e
  x -> emap (substExpHS new old) x

instance SubstExp MExp where
  substExp new old exp = case exp of
    MVar v | v == old -> new
    MFunAbs v _ _ | v == old -> exp
    MFunAbs v t b -> MFunAbs v t (subst b)
    MH t e -> MH t $ substExpMH new old e
    MS t e -> MS t $ substExpMS new old e
    x -> emap subst x
    where subst = substExp new old

substExpMH :: MExp -> EVar -> HExp -> HExp
substExpMH new old exp = case exp of
  HM t e -> HM t $ substExp new old e
  x -> emap (substExpMH new old) x

substExpMS :: MExp -> EVar -> SExp -> SExp
substExpMS new old exp = case exp of
  SM t e -> SM t $ substExp new old e
  x -> emap (substExpMS new old) x

instance SubstExp SExp where
  substExp new old exp = case exp of
    SVar v | v == old -> new
    SFunAbs v _ | v == old -> exp
    SFunAbs v b -> SFunAbs v (subst b)
    SH t e -> SH t $ substExpSH new old e
    SM t e -> SM t $ substExpSM new old e
    x -> emap subst x
    where subst = substExp new old

substExpSH :: SExp -> EVar -> HExp -> HExp
substExpSH new old exp = case exp of
  HS t e -> HS t $ substExp new old e
  x -> emap (substExpSH new old) x

substExpSM :: SExp -> EVar -> MExp -> MExp
substExpSM new old exp = case exp of
  MS t e -> MS t $ substExp new old e
  x -> emap (substExpSM new old) x
