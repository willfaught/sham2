module Check (check) where

import Context
import Subst
import Syntax

data SError e t = Unexpected {
    unexExp :: e,
    unexExpected :: t,
    unexActual :: t }
  | Mismatch {
    mismatchFstExp :: e,
    mismatchFstTy :: t,
    mismatchSndExp :: e,
    mismatchSndTy :: t }
  | Unequal {
    unequalFst :: t,
    unequalSnd :: t }
  | Wrong {
    wrongExp :: e,
    wrongTy :: t,
    wrongMsg :: Wrong }
  | Open {
    openTy :: t,
    openVar :: TVar }
  | Trace {
    traceOuter :: e,
    traceInner :: SError e t }
  deriving (Show, Eq)

data Wrong = ExpectingFunction
  | ExpectFunParamResEq
  | ParamArgMismatch
  deriving (Show, Eq)

class Check a b | a -> b where
  check :: Context -> a -> Either (SError a b) b

instance Check HExp SType where
  check = checkH

instance Check MExp SType where
  check = checkM

free :: Context -> SType -> Maybe TVar
free c t = case t of
  Lump -> Nothing
  Nat -> Nothing
  TyVar x -> if sbound x c then Nothing else return x
  Label x _ -> free c x
  Fun x y -> let
    p = free c x
    r = free c y in case p of
      Nothing -> r
      Just _ -> p
  Forall x y -> free (stbind x c) y

-- Haskell

checkH :: Context -> HExp -> Either (SError HExp SType) SType
checkH cxt exp = case exp of
  HAdd x y -> let
    t = check cxt x
    u = check cxt y in case t of
      Left e -> Left $ Trace exp e
      Right Nat -> case u of
        Left e -> Left $ Trace exp e
        Right Nat -> Right Nat
        Right u' -> Left $ Unexpected y Nat u'
      Right t' -> Left $ Unexpected x Nat t'
  HFix x -> let t = check cxt x in case t of
    Left e -> Left $ Trace exp e
    Right (Fun p r) | p == r -> Right p
    Right t' @ (Fun p r) -> Left (Trace exp (Wrong x t' ExpectFunParamResEq))
    Right t' -> Left . Trace exp $ Wrong x t' ExpectingFunction
  HFunAbs v t b -> let
    f = free cxt t
    u = check (sebind v t cxt) b in case f of
      Nothing -> case u of
        Left e -> Left $ Trace exp e
        Right u' -> Right $ Fun t u'
      Just f' -> Left . Trace exp $ Open t f'
  HFunApp x y -> let
    t = check cxt x
    u = check cxt y in case t of
      Left e -> Left $ Trace exp e
      Right (Fun p r) -> case u of
        Left e -> Left $ Trace exp e
        Right u' | u' == p -> Right u'
        Right u' -> Left . Trace exp $ Wrong y u' ParamArgMismatch
      Right t' -> Left (Trace exp (Wrong x t' ExpectingFunction))
  HIf0 x y z -> case check cxt x of
    Left e -> Left $ Trace exp e
    Right Nat -> case check cxt y of
      Left e -> Left $ Trace exp e
      Right y' -> case check cxt z of
        Left e -> Left $ Trace exp e
        Right z' | z' == y' -> Right z'
        Right z' -> Left . Trace exp $ Mismatch y y' z z'
    Right x' -> Left . Trace exp $ Unexpected x Nat x'
  HM t x -> case free cxt t of
    Just t' -> Left . Trace exp $ Open t t'
    Nothing -> case check cxt x of
      Left e -> Left $ Trace exp e
      Right x' | x' == t -> Right x'
      Right x' -> Left . Trace exp $ Unequal x' t
  
  --
{-HNum _ -> Just Nat
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
    return t-}

-- ML

checkM = undefined
{-checkM :: Context -> MExp -> Maybe SType
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
  return t-}

{-checkS :: [TyDef] -> Context -> SExp -> Maybe DType
checkS _ _ = undefined-}
