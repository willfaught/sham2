module Check (checkH, checkM) where

import Context
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Subst
import Syntax

closed :: Check SType a
closed t = case t of
  Lump -> return Lump
  Nat -> return Nat
  TyVar x -> return $ sbound x c
  Label x _ -> return $ closed x
  Fun x y -> return $ Fun (closed x) && (closed y)
  Forall x y -> return $ closed (stbind x c) y

assert :: Bool -> Maybe Bool
assert True = Just True
assert False = Nothing

data CheckError e t = Wrong {
    wrongExp :: e,
    wrongExpected :: t,
    wrongActual :: t }
  | Mismatch {
    mismatchFstExp :: e,
    mismatchFstTy :: t,
    mismatchSndExp :: e,
    mismatchSndTy :: t }
  deriving (Show, Eq)

class Check a b | a -> b where
  check :: Context -> a -> Either (CheckError a b) b

instance Check HExp SType where
  check = checkH

newtype Check t e = Check { runCheck :: Context -> Either (CheckError e t) t }

instance Monad (Check SType) where
  Check x >>= f = Check $ \ c -> case x c of
    Left e -> Left e
    Right t -> f t $ c
  x >> y = x >>= (\ _ -> y)
  return x = Check (\ _ -> Right x)

is :: a -> SType -> Check SType a
is e t1 = do
  t2 <- check e
  if t1 == t2 then return t1 else Left $ Wrong e t1 t2

matches :: a -> a -> Check SType a
matches e1 e2 = do
  t1 <- check e1
  t2 <- check e2
  if t1 == t2 then return t1 else Left $ Mismatch e1 t1 e2 t2

context :: 

-- Haskell

checkH :: Context -> HExp -> Either (CheckError HExp SType) SType
checkH cxt exp = case exp of
  HAdd x y -> do
    x `is` Nat
    y `is` Nat
    return Nat
  HFix x -> do
    x `satisfies` (\ x -> case x of Fun _ _ -> True ; _ -> False)
  HFunAbs v p e -> do
    closed p
    t <- check (sebind v p context) e
  HIf0 x y z -> do
    x `is` Nat
    y `matches` z
    return y

  --
  HAdd x y -> do
    t <- checkH cxt x
    u <- checkH cxt y
    assert (t == Nat && u == Nat)
    return Nat
  HFix x -> do
    t <- checkH cxt x
    case t of
      Fun p b -> Just p
      _ -> Nothing
  HFunAbs v p e -> do
    assert (closed p)
    b <- checkH (sebind v p cxt) e
    return (Fun p b)
  HFunApp x y -> do
    opr <- checkH cxt x
    opd <- checkH cxt y
    case opr of
      Fun p b -> do
        assert (p == opd)
        return b
      _ -> Nothing
  HIf0 g t f -> do
    gt <- checkH cxt g
    tt <- checkH cxt t
    ft <- checkH cxt f
    assert (gt == Nat && tt == ft)
    return tt
  HM t e -> do
    assert (closed t)
    u <- checkM cxt e
    assert (t == u)
    return t
  HNum _ -> Just Nat
  HSub x y -> do
    t <- checkH cxt x
    assert (t == Nat)
    u <- checkH cxt y
    assert (u == Nat)
    return Nat
  HTyAbs v e -> do
    t <- checkH (stbind v cxt) e
    return (Forall v t)
  HTyApp e t -> do
    assert (closed t)
    Forall v u <- checkH cxt e
    return $ substTy t v u
  HVar v -> sbinding v cxt
  HWrong t s -> do
    assert (closed t)
    return t

-- ML

checkM :: Context -> MExp -> Maybe SType
checkM c (MAdd x y) = do
  t <- checkM c x
  u <- checkM c y
  assert (t == Nat && u == Nat)
  return Nat
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
checkM c (MH t e) = do
  assert (closed t)
  u <- checkH c e
  assert (t == u)
  return t
checkM c (MIf0 g t f) = do
  gt <- checkM c g
  tt <- checkM c t
  ft <- checkM c f
  assert (gt == Nat && tt == ft)
  return tt
checkM _ (MNum _) = Just Nat
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
  return $ substTy t v u
checkM c (MVar v) = sbinding v c
checkM c (MWrong t s) = do
  assert (closed t)
  return t

{-checkS :: [TyDef] -> Context -> SExp -> Maybe DType
checkS _ _ = undefined-}
