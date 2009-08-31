module Syntax where

type EVar = String

type TVar = String

data DType =
  DType
  deriving (Eq, Show)

data SType =
  Ext String [SType]
  | Forall TVar SType
  | Fun SType SType
  | Label SType
  | Lump
  | Nat
  | TyVar TVar
  deriving (Eq, Show)

newtype ConTyRule t =
  ConTyRule ([t] -> t)

class Foo a b where
  foo :: a b

newtype FieldTyRule t =
  FieldTyRule (t -> t)

data TyField =
  TyField {
    tyfieldName :: Maybe String,
    tyfieldDTyRule :: FieldTyRule DType,
    tyfieldSTyRule :: FieldTyRule SType }

data TyCon =
  TyCon {
    tyconName :: String,
    tyconFields :: [TyField],
    tyconDTyRule :: ConTyRule DType,
    tyconSTyRule :: ConTyRule SType }

data TyDef =
  TyDef {
    tydefName :: String,
    tydefTypes :: Int,
    tydefCons :: [TyCon] }

data Field e =
  FieldExp e
  | FieldType SType
  deriving (Eq, Show)

data HExp =
  HAdd HExp HExp
  | HCon String [Field HExp]
  | HFix HExp
  | HFunAbs EVar SType HExp
  | HFunApp HExp HExp
  | HField String HExp
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
