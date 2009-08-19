module Sham where

type EVar = String

type TVar = String

--type ExpType = HExp -> Type

data TyField = TyField { tyfieldName :: String, tyfieldType :: SType } deriving (Eq, Show)

data TyCon = TyCon { tyconName :: String, tyconFields :: [TyField] } deriving (Eq, Show)

data TyDef = TyDef { tydefName :: String, tydefTypes :: [TVar], tydefCons :: [TyCon] } deriving (Eq, Show)

data DType = DType

data SType = Lump
  | Nat
  | TyVar TVar
  | Label SType
  | Fun SType SType
  | Forall TVar SType
  | Ext String [SType]
  deriving (Eq, Show)

data HExp = HVar EVar
  | HApp HExp HExp
  | HFix HExp
  | HTyApp HExp SType
  | HOp String HExp
  | HAdd HExp HExp
  | HSub HExp HExp
  | HIf0 HExp HExp HExp
  | HWrong SType String
  | HM SType MExp
  | HS SType SExp
  | HFunAbs EVar SType HExp
  | HTyAbs TVar HExp
  | HNum Integer
  deriving (Eq, Show)

data MExp = MNum Integer
  deriving (Eq, Show)

data SExp = SNum Integer
  deriving (Eq, Show)

--hExpType :: HContext -> HExpType
--hExpType c (HVar x) = 

unlabel :: SType -> SType
unlabel (Label t) = t
unlabel (Fun x y) = Fun (unlabel x) (unlabel y)
unlabel (Forall x y) = Forall x (unlabel y)
unlabel x = x

substType :: SType -> SType -> SType -> SType
substType _ _ Lump = Lump
substType _ _ Nat = Nat
substType new (TyVar x) (TyVar y) | x == y = new
substType _ _ x @ (Label _) = x
substType new old (Fun x y) = Fun (substType new old x) (substType new old y)
substType new old @ (TyVar x) forall @ (Forall y z) | x /= y = Forall y (substType new old z)
                                                    | otherwise = forall
--don't forget about Ext!
