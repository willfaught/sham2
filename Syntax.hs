module Syntax where

type EVar = String

type TVar = String

data DType =
  DType
  deriving (Eq, Show)

data SType =
  Lump
  | Nat
  | TyVar TVar
  | Label SType
  | Fun SType SType
  | Forall TVar SType
  | Ext String [SType]
  deriving (Eq, Show)

newtype ConTyRule t =
  ConTyRule ([t] -> t)

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

data HField =
  HFieldExp HExp
  | HFieldType SType
  deriving (Eq, Show)

data HExp =
  HAdd HExp HExp
  | HApp HExp HExp
  | HCon String [HField]
  | HFix HExp
  | HFunAbs EVar SType HExp
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
