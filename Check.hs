module Check (checkH, checkM) where

import Context
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Subst
import Syntax

closed :: SType -> Bool
closed t = go empty t where
  go c Lump = True
  go c Nat = True
  go c (TyVar x) = sbound x c
  go c (Label x _) = go c x
  go c (Fun x y) = go c x && go c y
  go c (Forall x y) = go (stbind x c) y
  --go c (Ext _ x) = and (map (go c) x)

assert :: Bool -> Maybe Bool
assert True = Just True
assert False = Nothing

-- Haskell

checkH :: Context -> HExp -> Maybe SType
checkH c (HAdd x y) = do
  t <- checkH c x
  u <- checkH c y
  assert (t == Nat && u == Nat)
  return Nat
{-checkH c (HCon n f1) = do -- need to unify tyvars here to determine type params for con's type
  TyCon _ f2 _ s <- tycon n d
  assert (length f1 == length f2)
  let fieldType (FieldExp x) = checkH c x
      fieldType (FieldType x) = do
        assert (closed x)
        return x
  t <- sequence $ map fieldType f1
  TyDef _ a _ <- condef n d
  assert (a == 
  let e @ (Ext _ a) = s t
  assert (length a == 
  return e-}
checkH c (HFix x) = do
  t <- checkH c x
  case t of
    Fun p b -> Just p
    _ -> Nothing
checkH c (HFunAbs v p e) = do
  assert (closed p)
  b <- checkH (sebind v p c) e
  return (Fun p b)
checkH c (HFunApp x y) = do
  opr <- checkH c x
  opd <- checkH c y
  case opr of
    Fun p b -> do
      assert (p == opd)
      return b
    _ -> Nothing
{-checkH c (HField n e) = do
  TyField _ _ s <- tyfield n d
  t <- checkH c e
  return $ s t-}
checkH c (HIf0 g t f) = do
  gt <- checkH c g
  tt <- checkH c t
  ft <- checkH c f
  assert (gt == Nat && tt == ft)
  return tt
{-checkH c (HM t m) = do
  assert (closed t)
  u <- checkM d c m
  assert (t == u)
  return t-}
checkH _ (HNum _) = Just Nat
{-checkH c (HS t e) = do
  assert (closed t)
  checkS d c e
  return t-}
checkH c (HSub x y) = do
  t <- checkH c x
  assert (t == Nat)
  u <- checkH c y
  assert (u == Nat)
  return Nat
checkH c (HTyAbs v e) = do
  t <- checkH (stbind v c) e
  return (Forall v t)
checkH c (HTyApp e t) = do
  assert (closed t)
  Forall v u <- checkH c e
  return $ substTyTy t v u
checkH c (HVar v) = sbinding v c
checkH c (HWrong t s) = do
  assert (closed t)
  return t

-- ML

checkM :: Context -> MExp -> Maybe SType
checkM c (MAdd x y) = do
  t <- checkM c x
  u <- checkM c y
  assert (t == Nat && u == Nat)
  return Nat
{-checkM c (MCon n f1) = do -- need to unify tyvars here to determine type params for con's type
  TyCon _ f2 _ s <- tycon n d
  assert (length f1 == length f2)
  let fieldType (FieldExp x) = checkM c x
      fieldType (FieldType x) = do
        assert (closed x)
        return x
  t <- sequence $ map fieldType f1
  TyDef _ a _ <- condef n d
  assert (a == 
  let e @ (Ext _ a) = s t
  assert (length a == 
  return e-}
checkM c (MFix x) = do
  t <- checkM c x
  case t of
    Fun p b -> Just p
    _ -> Nothing
checkM c (MFunAbs v p e) = do
  assert (closed p)
  b <- checkM (sebind v p c) e
  return (Fun p b)
checkM c (MFunApp x y) = do
  opr <- checkM c x
  opd <- checkM c y
  case opr of
    Fun p b -> do
      assert (p == opd)
      return b
    _ -> Nothing
{-checkM c (MField n e) = do
  TyField _ _ s <- tyfield n d
  t <- checkM c e
  return $ s t-}
checkM c (MIf0 g t f) = do
  gt <- checkM c g
  tt <- checkM c t
  ft <- checkM c f
  assert (gt == Nat && tt == ft)
  return tt
{-checkM c (MM t m) = do
  assert (closed t)
  u <- checkM d c m
  assert (t == u)
  return t-}
checkM _ (MNum _) = Just Nat
{-checkM c (MS t e) = do
  assert (closed t)
  checkS d c e
  return t-}
checkM c (MSub x y) = do
  t <- checkM c x
  assert (t == Nat)
  u <- checkM c y
  assert (u == Nat)
  return Nat
checkM c (MTyAbs v e) = do
  t <- checkM (stbind v c) e
  return (Forall v t)
checkM c (MTyApp e t) = do
  assert (closed t)
  Forall v u <- checkM c e
  return $ substTyTy t v u
checkM c (MVar v) = sbinding v c
checkM c (MWrong t s) = do
  assert (closed t)
  return t

{-checkS :: [TyDef] -> Context -> SExp -> Maybe DType
checkS _ _ = undefined-}
