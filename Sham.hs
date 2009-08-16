module Sham where

newtype EVar = EVar String deriving (Show, Eq)

newtype TVar = TVar String deriving (Show, Eq)

data TyField = TyField { tyfieldName :: String, tyfieldType :: Type } deriving (Eq, Show)

data TyCon = TyCon { tyconName :: String, tyconFields :: [TyField] } deriving (Eq, Show)

data TyDef = TyDef { tydefName :: String, tydefTypes :: [TVar], tydefCons :: [TyCon] } deriving (Eq, Show)

data Type = Lump
  | Nat
  | TyVar TVar
  | Label Type
  | Fun Type Type
  | Forall TVar Type
  | Ext TyDef
  deriving (Eq, Show)

data HExp = HVar EVar
  | HApp HExp HExp
  | HFix HExp
  | HTyApp HExp Type
  | HOp String HExp
  | HAdd HExp HExp
  | HSub HExp HExp
  | HIf0 HExp HExp HExp
  | HWrong Type String
  | HM Type MExp
  | HS Type SExp
  | HFunAbs EVar Type HExp
  | HTyAbs TVar HExp
  | HNum Integer
  deriving (Eq, Show)

data MExp = MNum Integer
  deriving (Eq, Show)

data SExp = SNum Integer
  deriving (Eq, Show)

unlabel :: Type -> Type
unlabel (Label t) = t
unlabel (Fun x y) = Fun (unlabel x) (unlabel y)
unlabel (Forall x y) = Forall x (unlabel y)
unlabel x = x

substType :: Type -> Type -> Type -> Type
substType _ _ Lump = Lump
substType _ _ Nat = Nat
substType new (TyVar x) (TyVar y) | x == y = new
substType _ _ x @ (Label _) = x
substType new old (Fun x y) = Fun (substType new old x) (substType new old y)
substType new old @ (TyVar x) forall @ (Forall y z) | x /= y = Forall y (substType new old z)
                                                    | otherwise = forall
