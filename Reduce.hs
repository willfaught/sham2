module Reduce (reduce, reducible, lazy, eager) where

import Control.Monad.State
import Data.Either
import Substitute
import Syntax

type Reduction t = Either String t

class Reduce t where
  reduce :: t -> Reduction t
  reducible :: t -> Bool
  lazy :: t -> Bool
  eager :: t -> Bool

newtype ReductionState t = ReductionState (Either String t)

instance Monad ReductionState where
  ReductionState x >>= f = case x of
    Left s -> ReductionState . Left $ s
    Right e -> f e
  return = ReductionState . Right
  fail = ReductionState . Left

unlabel :: SType -> SType
unlabel t = case t of
  Label u _ -> u
  Fun p r -> Fun (unlabel p) (unlabel r)
  Forall v b -> Forall v (unlabel b)
  _ -> t

-- Haskell

instance Reduce HExp where
  reduce e = let ReductionState r = evalStateT (reduceH e) 1 in r
  reducible (HVar _) = False
  reducible e = not . lazy $ e
  lazy exp = case exp of
    HFunAbs _ _ _ -> True
    HTyAbs _ _ -> True
    HNum _ -> True
    HS Lump e | lazy e -> True
    HS (Forall _ _) e | lazy e -> True
    _ -> False
  eager = undefined

reduceH :: HExp -> StateT Int ReductionState HExp
reduceH exp = case exp of
  HAdd (HNum x) (HNum y) -> return . HNum $ x + y
  HAdd x y | not $ lazy x -> do
    x' <- reduceH x
    return $ HAdd x' y
  HAdd x y -> do
    y' <- reduceH y
    return $ HAdd x y'
  HFix (HFunAbs v _ b) -> return $ substExp exp v b
  HFix x -> do
    x' <- reduceH x
    return $ HFix x'
  HFunApp (HFunAbs v _ b) a -> return $ substExp a v b
  HFunApp x y -> do
    x' <- reduceH x
    return $ HFunApp x' y
  HIf0 (HNum n) t f -> return $ if n == 0 then t else f
  HIf0 x t f -> do
    x' <- reduceH x
    return $ HIf0 x' t f
  HM t (MH t' e) | t == t' -> return e
  HM Lump (MS Lump e) | lazy e -> return $ HS Lump e
  HM Nat (MNum n) -> return $ HNum n
  HM (Fun p r) f @ (MFunAbs v t b) | p == t -> return $ HFunAbs v p (HM r (MFunApp f (MH p (HVar v))))
  HM (Forall v t) (MTyAbs v' e) | v == v'-> return $ HTyAbs v (HM t e)
  HM f @ (Forall v t) (MS (Forall v' t') e) | v == v' && t == t' && lazy e -> return $ HS f e
  HM t e | not $ lazy e -> do
    e' <- reduceM e
    return $ HM t e'
  HS t (SH t' e) | t == t' -> return e
  HS Nat (SNum n) -> return $ HNum n
  HS Nat e | lazy e -> return $ HWrong Nat "Not a number"
  HS (Label t _) e | lazy e -> return $ HWrong t "Parametricity violated"
  HS (Fun p r) f @ (SFunAbs v b) -> return $ HFunAbs v (unlabel p) (HS r (SFunApp f (SH p (HVar v))))
  HS t @ (Fun _ _) e | lazy e -> return $ HWrong (unlabel t) "Not a function"
  HS t e | not $ lazy e -> do
    e' <- reduceS e
    return $ HS t e'
  HSub (HNum x) (HNum y) -> return . HNum $ max 0 $ x - y
  HSub x y | not $ lazy x -> do
    x' <- reduceH x
    return $ HSub x' y
  HSub x y -> do
    y' <- reduceH y
    return $ HSub x y'
  HTyApp (HTyAbs v b) t -> return $ substTyExp t v b
  HTyApp (HS (Forall v t) e) t' | lazy e -> do
    label <- get
    put (label + 1)
    return $ HS (substTy (Label t' label) v t) e
  HTyApp x t -> do
    x' <- reduceH x
    return $ HTyApp x' t
  HWrong _ s -> fail s
  _ -> error $ "Irreducible or invalid expression: " ++ show exp

-- ML

instance Reduce MExp where
  reduce e = let ReductionState r = evalStateT (reduceM e) 1 in r
  reducible (MVar _) = False
  reducible e = (not . lazy $ e) && (not . eager $ e)
  lazy exp = case exp of
    MH _ _ -> True
    _ -> eager exp
  eager exp = case exp of
    MFunAbs _ _ _ -> True
    MTyAbs _ _ -> True
    MNum _ -> True
    MS Lump e | lazy e -> True
    MS (Forall _ _) e | lazy e -> True
    _ -> False

reduceM :: MExp -> StateT Int ReductionState MExp
reduceM exp = case exp of
  MAdd (MNum x) (MNum y) -> return . MNum $ x + y
  MAdd x y | not $ eager x -> do
    x' <- reduceForceM x
    return $ MAdd x' y
  MAdd x y -> do
    y' <- reduceForceM y
    return $ MAdd x y'
  MFix (MFunAbs v _ b) -> return $ substExp exp v b
  MFix x -> do
    x' <- reduceForceM x
    return $ MFix x'
  MFunApp (MFunAbs v t b) a | lazy a -> return $ substExp a v b
  MFunApp x y | not $ eager x -> do
    x' <- reduceForceM x
    return $ MFunApp x' y
  MFunApp x y -> do
    y' <- reduceForceM y
    return $ MFunApp x y'
  MIf0 (MNum n) t f -> return $ if n == 0 then t else f
  MIf0 x t f -> do
    x' <- reduceForceM x
    return $ MIf0 x' t f
  --MH t (HM t' e) | t == t' -> return e
  MH Lump (HS Lump e) | lazy e -> return $ MS Lump e
  MH Nat (HNum n) -> return $ MNum n
  MH (Fun p r) f @ (HFunAbs v t b) | p == t -> return $ MFunAbs v p (MH r (HFunApp f (HM p (MVar v))))
  MH (Forall v t) (HTyAbs v' e) | v == v'-> return $ MTyAbs v (MH t e)
  MH f @ (Forall v t) (HS (Forall v' t') e) | v == v' && t == t' && lazy e -> return $ MS f e
  MS t (SM t' e) | t == t' -> return e
  MS Nat (SNum n) -> return $ MNum n
  MS Nat e | lazy e -> return $ MWrong Nat "Not a number"
  MS (Label t _) e | lazy e -> return $ MWrong t "Parametricity violated"
  MS (Fun p r) f @ (SFunAbs v b) -> return $ MFunAbs v (unlabel p) (MS r (SFunApp f (SM p (MVar v))))
  MS t @ (Fun _ _) e | lazy e -> return $ MWrong (unlabel t) "Not a function"
  MS t e | not $ lazy e -> do
    e' <- reduceS e
    return $ MS t e'
  MSub (MNum x) (MNum y) -> return . MNum $ max 0 $ x - y
  MSub x y | not $ eager x -> do
    x' <- reduceForceM x
    return $ MSub x' y
  MSub x y -> do
    y' <- reduceForceM y
    return $ MSub x y'
  MTyApp (MTyAbs v b) t -> return $ substTyExp t v b
  MTyApp (MS (Forall v t) e) t' | lazy e -> do
    label <- get
    put (label + 1)
    return $ MS (substTy (Label t' label) v t) e
  MTyApp x y -> do
    x' <- reduceForceM x
    return $ MTyApp x' y
  MWrong _ s -> fail s
  _ -> error $ "Irreducible or invalid expression: " ++ show exp

reduceForceM :: MExp -> StateT Int ReductionState MExp
reduceForceM exp = case exp of
  MH t e | lazy e -> do
    e' <- reduceH e
    return $ MH t e'
  _ -> reduceM exp

-- Scheme

instance Reduce SExp where
  reduce e = let ReductionState r = evalStateT (reduceS e) 1 in r
  reducible (SVar _) = False
  reducible e = (not . lazy $ e) && (not . eager $ e)
  lazy exp = case exp of
    SH _ _ -> True
    _ -> eager exp
  eager exp = case exp of
    SFunAbs _ _ -> True
    SNum _ -> True
    SM (Label _ _) e | lazy e -> True
    _ -> False

reduceS :: SExp -> StateT Int ReductionState SExp
reduceS = undefined
{-reduceS exp = case exp of
  SAdd (SNum x) (SNum y) -> return . SNum $ x + y
  SAdd x y | not $ eager x -> do
    x' <- reduceForceS x
    return $ SAdd x' y
  SAdd x y | not $ eager y -> do
    y' <- reduceForceS y
    return $ SAdd x y'
  SAdd _ _ -> return $ SWrong "Not a number"
  SFunApp (SFunAbs v e) e' | lazy e' -> return $ substExp e' v e
  SFunApp x y | not $ eager x -> do
    x' <- reduceForceS x
    return $ SFunApp x' y
  SFunApp x y | not $ lazy y -> do
    y' <- reduceForceS y
    return $ SFunApp x y'
  SFunApp _ _ -> return $ SWrong "Not a function"
  SIf0 (SNum n) t f -> return $ if n == 0 then t else f
  SIf0 x t f | not $ eager x -> do
    x' <- reduceForceS x
    return $ SIf0 x' t f
  SIf0 _ _ _ -> return $ SWrong "Not a number"
  --SH t (HS t' e) | t == t' -> return e
  SH Lump (HS Lump e) | lazy e -> return e
  SH Nat (HNum n) -> return $ SNum n
  SH (Fun p r) f @ (HFunAbs v t b) | p == t -> return $ SFunAbs v (SH r (HFunApp f (HS p (SVar v))))
  --SH (Forall v t) (HTyAbs v' e) | v == v' -> return $ STyAbs v (SH t e)
  --SH f @ (Forall v t) (HS (Forall v' t') e) | v == v' && t == t' && lazy e -> return $ SS f e
  SM t (SM t' e) | t == t' -> return e
  SM Nat (SNum n) -> return $ SNum n
  SM Nat e | lazy e -> return $ SWrong Nat "Not a number"
  SM (Label t _) e | lazy e -> return $ SWrong t "Parametricity violated"
  SM (Fun p r) f @ (SFunAbs v b) -> return $ SFunAbs v (unlabel p) (SM r (SFunApp f (SM p (SVar v))))
  SM t @ (Fun _ _) e | lazy e -> return $ SWrong (unlabel t) "Not a function"
  SM t e | not $ lazy e -> do
    e' <- reduceS e
    return $ SM t e'
  SSub (SNum x) (SNum y) -> return . SNum $ max 0 $ x - y
  SSub x y | not $ eager x -> do
    x' <- reduceForceS x
    return $ SSub x' y
  SSub x y | not $ eager y -> do
    y' <- reduceForceS y
    return $ SSub x y'
  SSub _ _ -> return $ SWrong "Not a number"
  SWrong _ s -> fail s
  _ -> irreducible

reduceForceS :: SExp -> StateT Int ReductionState SExp
reduceForceS exp = case exp of
  SH t e | lazy e -> do
    e' <- reduceH e
    return $ SH t e'
  _  -> reduceS exp
-}
