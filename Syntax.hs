module Syntax where

import Data.List (find)
import Data.Maybe (fromJust, isJust)

type EVar = String

type TVar = String

data DType = DType
  deriving (Eq, Show)

data SType = Forall TVar SType
  --Ext Name [SType]
  | Fun SType SType
  | Label SType Int
  | Lump
  | Nat
  | TyVar TVar
  deriving Eq

instance Show SType where
  show (Forall v t) = "A" ++ show v ++ "." ++ show t
  show (Fun x y) = show x ++ "->" ++ show y
  show (Label t n) = show t ++ "^" ++ show n
  show Lump = "L"
  show Nat = "N"
  show (TyVar v) = show v

type Name = String

{-data TyField =
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
  -- | HCon Name [Field HExp]
  | HFix HExp
  | HFunAbs EVar SType HExp
  | HFunApp HExp HExp
  -- | HField Name HExp
  | HIf0 HExp HExp HExp
  -- | HM SType MExp
  | HNum Integer
  -- | HS SType SExp
  | HSub HExp HExp
  | HTyAbs TVar HExp
  | HTyApp HExp SType
  | HVar EVar
  | HWrong SType String
  deriving Eq

instance Show HExp where
  show (HAdd x y) = "+ " ++ show x ++ " " ++ show y
  show (HFix x) = "fix " ++ show x
  show (HFunAbs v t b) = "\\" ++ show v ++ ":" ++ show t ++ "." ++ show b
  show (HFunApp x y) = show x ++ " " ++ show y
  show (HIf0 x y z) = "if0 " ++ show x ++ " " ++ show y ++ " " ++ show z
  show (HNum x) = show x
  show (HSub x y) = "- " ++ show x ++ " " ++ show y
  show (HTyAbs v b) = "\\\\" ++ show v ++ "." ++ show b
  show (HTyApp e t) = show e ++ " {" ++ show t ++ "}"
  show (HVar v) = show v
  show (HWrong t s) = "wrong " ++ show t ++ " " ++ s

data MExp =
  MAdd MExp MExp
  -- | MCon Name [Field MExp]
  | MFix MExp
  | MFunAbs EVar SType MExp
  | MFunApp MExp MExp
  -- | MField Name MExp
  | MIf0 MExp MExp MExp
  -- | MM SType MExp
  | MNum Integer
  -- | MS SType SExp
  | MSub MExp MExp
  | MTyAbs TVar MExp
  | MTyApp MExp SType
  | MVar EVar
  | MWrong SType String
  deriving Eq

instance Show MExp where
  show (MAdd x y) = "+ " ++ show x ++ " " ++ show y
  show (MFix x) = "fix " ++ show x
  show (MFunAbs v t b) = "\\" ++ show v ++ ":" ++ show t ++ "." ++ show b
  show (MFunApp x y) = show x ++ " " ++ show y
  show (MIf0 x y z) = "if0 " ++ show x ++ " " ++ show y ++ " " ++ show z
  show (MNum x) = show x
  show (MSub x y) = "- " ++ show x ++ " " ++ show y
  show (MTyAbs v b) = "\\\\" ++ show v ++ "." ++ show b
  show (MTyApp e t) = show e ++ " {" ++ show t ++ "}"
  show (MVar v) = show v
  show (MWrong t s) = "wrong " ++ show t ++ " " ++ s

{-data SExp =
  SNum Integer
  deriving (Eq, Show)-}
