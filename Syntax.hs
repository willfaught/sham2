module Syntax where

class ExpMap a where
  emap :: (a -> a) -> a -> a

type EVar = String

-- Type

data DType = DType deriving (Eq, Show)

type TVar = String

data SType = Forall TVar SType
  | Fun SType SType
  | Label SType Int
  | Lump
  | Nat
  | TyVar TVar
  deriving Eq

instance Show SType where
  show = showT True

showT :: Bool -> SType -> String
showT top e = case e of
  Forall v t -> wrap $ "A" ++ v ++ "." ++ showT False t
  Fun x y -> wrap $ showT False x ++ "->" ++ showT False y
  Label t n -> wrap $ showT False t ++ "^" ++ show n
  Lump -> "L"
  Nat -> "N"
  TyVar v -> v
  where wrap s = if top then s else "(" ++ s ++ ")"

-- Haskell

data HExp = HAdd HExp HExp
  | HFix HExp
  | HFunAbs EVar SType HExp
  | HFunApp HExp HExp
  | HIf0 HExp HExp HExp
  | HM SType MExp
  | HNum Integer
  | HS SType SExp
  | HSub HExp HExp
  | HTyAbs TVar HExp
  | HTyApp HExp SType
  | HVar EVar
  | HWrong SType String
  deriving Eq

instance Show HExp where
  show = showH True

showH :: Bool -> HExp -> String
showH top e = case e of
  HAdd x y -> wrap $ "+ " ++ showH False x ++ " " ++ showH False y
  HFix x -> wrap $ "fix " ++ showH False x
  HFunAbs v t b -> wrap $ "\\" ++ v ++ ":" ++ showT False t ++ "." ++ showH False b
  HFunApp x y -> wrap $ showH False x ++ " " ++ showH False y
  HIf0 x y z -> wrap $ "if0 " ++ showH False x ++ " " ++ showH False y ++ " " ++ showH False z
  HM t e -> wrap $ "HM " ++ showT False t ++ " " ++ showM False e
  HNum n -> show n
  HS t e -> wrap $ "HS " ++ showT False t ++ " " ++ showS False e
  HSub x y -> wrap $ "- " ++ showH False x ++ " " ++ showH False y
  HTyAbs v b -> wrap $ "\\\\" ++ v ++ "." ++ showH False b
  HTyApp e t -> wrap $ "" ++ showH False e ++ " {" ++ showT False t ++ "}"
  HVar v -> v
  HWrong t s -> wrap $ "wrong " ++ showT False t ++ " " ++ show s
  where wrap s = if top then s else "(" ++ s ++ ")"

instance ExpMap HExp where
  emap f e = case e of
    HAdd x y -> HAdd (f x) (f y)
    HFix x -> HFix $ f x
    HFunAbs v t x -> HFunAbs v t (f x)
    HFunApp x y -> HFunApp (f x) (f y)
    HIf0 x y z -> HIf0 (f x) (f y) (f z)
    HSub x y -> HSub (f x) (f y)
    HTyAbs v x -> HTyAbs v $ f x
    HTyApp x t -> HTyApp (f x) t
    _ -> e

-- ML

data MExp = MAdd MExp MExp
  | MFix MExp
  | MFunAbs EVar SType MExp
  | MFunApp MExp MExp
  | MH SType HExp
  | MIf0 MExp MExp MExp
  | MNum Integer
  | MS SType SExp
  | MSub MExp MExp
  | MTyAbs TVar MExp
  | MTyApp MExp SType
  | MVar EVar
  | MWrong SType String
  deriving Eq

instance ExpMap MExp where
  emap f e = case e of
    MAdd x y -> MAdd (f x) (f y)
    MFix x -> MFix $ f x
    MFunAbs v t x -> MFunAbs v t (f x)
    MFunApp x y -> MFunApp (f x) (f y)
    MIf0 x y z -> MIf0 (f x) (f y) (f z)
    MSub x y -> MSub (f x) (f y)
    MTyAbs v x -> MTyAbs v $ f x
    MTyApp x t -> MTyApp (f x) t
    _ -> e
    where go = emap f

instance Show MExp where
  show = showM True
  
showM :: Bool -> MExp -> String
showM top e = case e of
  MAdd x y -> wrap $ "+ " ++ showM False x ++ " " ++ showM False y
  MFix x -> wrap $ "fix " ++ showM False x
  MFunAbs v t b -> wrap $ "\\" ++ v ++ ":" ++ showT False t ++ "." ++ showM False b
  MFunApp x y -> wrap $ showM False x ++ " " ++ showM False y
  MH t e -> wrap $ "MH " ++ showT False t ++ " " ++ showH False e
  MIf0 x y z -> wrap $ "if0 " ++ showM False x ++ " " ++ showM False y ++ " " ++ showM False z
  MNum n -> show n
  MS t e -> wrap $ "MS " ++ showT False t ++ " " ++ showS False e
  MSub x y -> wrap $ "- " ++ showM False x ++ " " ++ showM False y
  MTyAbs v b -> wrap $ "\\\\" ++ v ++ "." ++ showM False b
  MTyApp e t -> wrap $ "" ++ showM False e ++ " {" ++ showT False t ++ "}"
  MVar v -> v
  MWrong t s -> wrap $ "wrong " ++ showT False t ++ " " ++ show s
  where wrap s = if top then s else "(" ++ s ++ ")"

-- Scheme

data SExp = SAdd SExp SExp
  | SFunAbs EVar SExp
  | SFunApp SExp SExp
  | SFunPred SExp
  | SH SType HExp
  | SIf0 SExp SExp SExp
  | SM SType MExp
  | SNum Integer
  | SNumPred SExp
  | SSub SExp SExp
  | SVar EVar
  | SWrong String
  deriving Eq

instance Show SExp where
  show = showS True

showS :: Bool -> SExp -> String
showS top e = case e of
  SAdd x y -> wrap $ "+ " ++ showS False x ++ " " ++ showS False y
  SFunAbs v b -> wrap $ "\\" ++ v ++ "." ++ showS False b
  SFunApp x y -> wrap $ showS False x ++ " " ++ showS False y
  SFunPred x -> wrap $ "fun? " ++ showS False x
  SH t e -> wrap $ "SH " ++ showT False t ++ " " ++ showH False e
  SIf0 x y z -> wrap $ "if0 " ++ showS False x ++ " " ++ showS False y ++ " " ++ showS False z
  SM t e -> wrap $ "SM " ++ showT False t ++ " " ++ showM False e
  SNum n -> show n
  SNumPred x -> wrap $ "num? " ++ showS False x
  SSub x y -> wrap $ "- " ++ showS False x ++ " " ++ showS False y
  SVar v -> v
  SWrong s -> wrap $ "wrong " ++ show s
  where wrap s = if top then s else "(" ++ s ++ ")"

instance ExpMap SExp where
  emap f e = case e of
    SAdd x y -> SAdd (f x) (f y)
    SFunAbs v x -> SFunAbs v (f x)
    SFunApp x y -> SFunApp (f x) (f y)
    SFunPred x -> SFunPred (f x)
    SIf0 x y z -> SIf0 (f x) (f y) (f z)
    SNumPred x -> SFunPred (f x)
    SSub x y -> SSub (f x) (f y)
    _ -> e
