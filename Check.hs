module Check (checkT, checkH, checkM, checkS) where

import Context
import Substitute
import Syntax

assert :: Bool -> Maybe Bool
assert True = Just True
assert False = Nothing

-- Type

checkT :: Context -> SType -> Bool
checkT c t = case t of
    Lump -> True
    Nat -> True
    TyVar x -> sbound x c
    Label x _ -> checkT c x
    Fun x y -> checkT c x && checkT c y
    Forall x y -> checkT (stbind x c) y

-- Haskell

checkH :: Context -> HExp -> Maybe SType
checkH cxt exp = case exp of
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
    assert (checkT cxt p)
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
    assert (checkT cxt t)
    u <- checkM cxt e
    assert (t == u)
    return t
  HNum _ -> Just Nat
  HS t e -> do
    assert (checkT cxt t)
    checkS cxt e
    return t
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
    assert (checkT cxt t)
    Forall v u <- checkH cxt e
    return $ substTy t v u
  HVar v -> sbinding v cxt
  HWrong t s -> do
    assert (checkT cxt t)
    return t

-- ML

checkM :: Context -> MExp -> Maybe SType
checkM cxt exp = case exp of
  MAdd x y -> do
    t <- checkM cxt x
    u <- checkM cxt y
    assert (t == Nat && u == Nat)
    return Nat
  MFix x -> do
    t <- checkM cxt x
    case t of
      Fun p b -> Just p
      _ -> Nothing
  MFunAbs v p e -> do
    assert (checkT cxt p)
    b <- checkM (sebind v p cxt) e
    return (Fun p b)
  MFunApp x y -> do
    opr <- checkM cxt x
    opd <- checkM cxt y
    case opr of
      Fun p b -> do
        assert (p == opd)
        return b
      _ -> Nothing
  MH t e -> do
    assert (checkT cxt t)
    u <- checkH cxt e
    assert (t == u)
    return t
  MIf0 g t f -> do
    gt <- checkM cxt g
    tt <- checkM cxt t
    ft <- checkM cxt f
    assert (gt == Nat && tt == ft)
    return tt
  MNum _ -> Just Nat
  MS t e -> do
    assert (checkT cxt t)
    checkS cxt e
    return t
  MSub x y -> do
    t <- checkM cxt x
    assert (t == Nat)
    u <- checkM cxt y
    assert (u == Nat)
    return Nat
  MTyAbs v e -> do
    t <- checkM (stbind v cxt) e
    return (Forall v t)
  MTyApp e t -> do
    assert (checkT cxt t)
    Forall v u <- checkM cxt e
    return $ substTy t v u
  MVar v -> sbinding v cxt
  MWrong t s -> do
    assert (checkT cxt t)
    return t

-- Scheme

checkS :: Context -> SExp -> Maybe DType
checkS cxt exp = case exp of
  SAdd x y -> do
    checkS cxt x
    checkS cxt y
    return DType
  SFunAbs v e -> do
    checkS (dbind v DType cxt) e
    return DType
  SFunApp x y -> do
    checkS cxt x
    checkS cxt y
    return DType
  SH t e -> do
    assert (checkT cxt t)
    e' <- checkH cxt e
    assert (t == e')
    return DType
  SIf0 g t f -> do
    checkS cxt g
    checkS cxt t
    checkS cxt f
    return DType
  SM t e -> do
    assert (checkT cxt t)
    e' <- checkM cxt e
    assert (t == e')
    return DType
  SNum _ -> return DType
  SSub x y -> do
    checkS cxt x
    checkS cxt y
    return DType
  SVar v -> dbinding v cxt
  SWrong s -> return DType
