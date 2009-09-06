module Check (
  checkH,
  checkM,
  checkS)
  where

import Context
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Syntax

unlabel :: SType -> SType
unlabel (Label t) = t
unlabel (Fun x y) = Fun (unlabel x) (unlabel y)
unlabel (Forall x y) = Forall x (unlabel y)
unlabel x = x

substType :: SType -> SType -> SType -> SType
substType new old (Ext n t) = Ext n $ map (substType new old) t
substType new old @ (TyVar x) forall @ (Forall y z) | x /= y = Forall y (substType new old z)
                                                    | otherwise = forall
substType new old (Fun x y) = Fun (substType new old x) (substType new old y)
substType _ _ x @ (Label _) = x
substType _ _ Lump = Lump
substType _ _ Nat = Nat
substType new (TyVar x) (TyVar y) | x == y = new

closed :: SType -> Bool
closed t = go empty t
  where go c Lump = True
        go c Nat = True
        go c (TyVar x) = sbound x c
        go c (Label x) = go c x
        go c (Fun x y) = go c x && go c y
        go c (Forall x y) = go (stbind x c) y
        go c (Ext _ x) = and (map (go c) x)

assert :: Bool -> Maybe Bool
assert True = Just True
assert False = Nothing

checkH :: [TyDef] -> Context -> HExp -> Maybe SType
checkH d c (HAdd x y) = do
  xt <- checkH d c x
  yt <- checkH d c y
  assert (xt == Nat && yt == Nat)
  return Nat
checkH d c (HCon n1 f1) = do
  let con x @ (TyCon n2 _ _ _) = if n1 == n2 then Just x else Nothing
  let def (TyDef _ _ c) = map con c
  TyCon _ f2 _ s <- fromJust $ find isJust $ concat $ map def d
  assert (length f1 == length f2)
  let fieldType (FieldExp x) = checkH d c x
      fieldType (FieldType x) = do
        assert (closed x)
        return x
  t <- sequence $ map fieldType f1
  return $ s t
checkH d c (HFix x) = do
  t <- checkH d c x
  case t of
    Fun p b -> Just p
    _ -> Nothing
checkH d c (HFunAbs v p e) =
  if closed p
  then do
    b <- checkH d (sebind v p c) e
    return (Fun p b)
  else Nothing
checkH d c (HFunApp x y) = do
  opr <- checkH d c x
  opd <- checkH d c y
  case opr of
    Fun p b -> do
      assert (p == opd)
      return b
    _ -> Nothing
checkH d c (HField n1 e) = do
  let field x @ (TyField (Just n2) _ _) = if n1 == n2 then Just x else Nothing
  let field (TyField Nothing _ _) = Nothing
  let con (TyCon _ f _ _) = map field f
  let def (TyDef _ _ c) = map con c
  TyField _ _ s <- fromJust $ find isJust $ concat $ concat $ map def d
  t <- checkH d c e
  return $ s t
checkH d c (HIf0 g t f) = do
  gt <- checkH d c g
  tt <- checkH d c t
  ft <- checkH d c f
  assert (gt == Nat && tt == ft)
  return tt
checkH d c (HM t m) = do
  assert (closed t)
  u <- checkM d c m
  assert (t == u)
  return t
checkH _ _ (HNum _) = Just Nat
checkH d c (HS t e) = do
  assert (closed t)
  checkS d c e
  return t
checkH d c (HSub x y) = do
  t <- checkH d c x
  assert (t == Nat)
  u <- checkH d c y
  assert (u == Nat)
  return Nat
checkH d c (HTyAbs v e) = do
  t <- checkH d (stbind v c) e
  return (Forall v t)
checkH d c (HTyApp e t) = do
  assert (closed t)
  Forall v u <- checkH d c e
  return $ substType t (TyVar v) u
checkH _ c (HVar v) = sbinding v c
checkH d c (HWrong t s) = do
  assert (closed t)
  return t

checkM :: [TyDef] -> Context -> MExp -> Maybe SType
checkM _ _ = undefined

checkS :: [TyDef] -> Context -> SExp -> Maybe DType
checkS _ _ = undefined
