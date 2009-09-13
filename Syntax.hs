module Syntax where

import Data.List (find)
import Data.Maybe (fromJust, isJust)

type EVar = String

type TVar = String

data DType =
  DType
  deriving (Eq, Show)

type Name = String

data SType =
  Ext Name [SType]
  | Forall TVar SType
  | Fun SType SType
  | Label SType
  | Lump
  | Nat
  | TyVar TVar
  deriving (Eq, Show)

type ConTyRule t = [t] -> t

type FieldTyRule t = t -> t

data TyField =
  TyField {
    tyfieldName :: Maybe Name,
    tyfieldDTyRule :: FieldTyRule DType,
    tyfieldSTyRule :: FieldTyRule SType }

data TyCon =
  TyCon {
    tyconName :: Name,
    tyconFields :: [TyField],
    tyconDTyRule :: ConTyRule DType,
    tyconSTyRule :: ConTyRule SType }

data TyDef =
  TyDef {
    tydefName :: Name,
    tydefArity :: Int,
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
  match (TyField (Just n2) _ _) = n1 == n2
  match (TyField Nothing _ _) = False

condef :: Name -> TyDefs -> Maybe TyDef
condef n = find $ any ((==) n . tyconName) . tydefCons

fieldcon :: Name -> TyDefs -> Maybe TyCon
fieldcon n1 = fromJust . find isJust . map def where
  def = find con . tydefCons
  con = any field . tyconFields
  field (TyField (Just n2) _ _) = n1 == n2
  field (TyField Nothing _ _) = False

data Field e =
  FieldExp e
  | FieldType SType
  deriving (Eq, Show)

data HExp =
  HAdd HExp HExp
  | HCon Name [Field HExp]
  | HFix HExp
  | HFunAbs EVar SType HExp
  | HFunApp HExp HExp
  | HField Name HExp
  | HIf0 HExp HExp HExp
  | HM SType MExp
  | HNum Integer
  | HS SType SExp
  | HSub HExp HExp
  | HTyAbs TVar HExp
  | HTyApp HExp SType
  | HVar EVar
  | HWrong SType String
  deriving (Eq, Show)

data MExp =
  MNum Integer
  deriving (Eq, Show)

data SExp =
  SNum Integer
  deriving (Eq, Show)
