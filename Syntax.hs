module Syntax where

import Data.List (find)
import Data.Maybe (fromJust, isJust)

type EVar = String

type TVar = String

data DType = DType deriving (Eq, Show)

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

{-type Name = String

data TyField =
  TyField {
    tyfieldName :: Maybe Name,
    tyfieldType :: SType }

data TyCon =
  TyCon {
    tyconName :: Name,
    tyconFields :: [TyField] }

data TyDef =
  TyDef {
    tydefName :: Name,
    tydefVars :: [TVar],
    tydefCons :: [TyCon] }

type TyDefs = [TyDef]

tydef :: Name -> TyDefs -> Maybe TyDef
tydef n = find $ (== n) . tydefName

tycon :: Name -> TyDefs -> Maybe TyCon
tycon n = fromJust . find isJust . map ((find $ (== n) . tyconName) . tydefCons)

tyfield :: Name -> TyDefs -> Maybe TyField
tyfield n1 = fromJust . find isJust . concat . map def where
  def = map con . tydefCons 
  con = find match . tyconFields
  match (TyField (Just n2) _) = n1 == n2
  match (TyField Nothing _) = False

condef :: Name -> TyDefs -> Maybe TyDef
condef n = find $ any ((==) n . tyconName) . tydefCons

fieldcon :: Name -> TyDefs -> Maybe TyCon
fieldcon n1 = fromJust . find isJust . map def where
  def = find con . tydefCons
  con = any field . tyconFields
  field (TyField (Just n2) _) = n1 == n2
  field (TyField Nothing _) = False

data Field e =
  FieldExp e
  | FieldType SType
  deriving (Eq, Show)-}

data HExp =
  HAdd HExp HExp
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

data MExp =
  MAdd MExp MExp
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

data SExp =
  SAdd SExp SExp
  | SFunAbs EVar SExp
  | SFunApp SExp SExp
  | SH SType HExp
  | SIf0 SExp SExp SExp
  | SM SType MExp
  | SNum Integer
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
  SH t e -> wrap $ "SH " ++ showT False t ++ " " ++ showH False e
  SIf0 x y z -> wrap $ "if0 " ++ showS False x ++ " " ++ showS False y ++ " " ++ showS False z
  SM t e -> wrap $ "SM " ++ showT False t ++ " " ++ showM False e
  SNum n -> show n
  SSub x y -> wrap $ "- " ++ showS False x ++ " " ++ showS False y
  SVar v -> v
  SWrong s -> wrap $ "wrong " ++ show s
  where wrap s = if top then s else "(" ++ s ++ ")"
