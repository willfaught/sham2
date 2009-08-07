module Sham where

data Type = Lump
  | Nat
  | TyVar String
  | Label Type
  | Fun Type Type
  | Forall Type Type

data HExp = HVar String
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
  | HFunAbs HExp Type HExp
  | HTyAbs Type HExp
  | HNum Integer

data MExp = MVar String
  | MApp MExp MExp
  | MFix MExp
  | MTyApp MExp Type
  | MField String MExp
  | MAdd MExp MExp
  | MSub MExp MExp
  | MIf0 MExp MExp MExp
  | MWrong Type String
  | MH Type HExp
  | MS Type SExp
  | MFunAbs MExp Type MExp
  | MTyAbs Type MExp
  | MNum Integer

data SExp = SVar String
  | SApp SExp SExp
  | SField String SExp
  | SAdd SExp SExp
  | SSub SExp SExp
  | SIf0 SExp SExp SExp
  | SWrong String
  | SH Type HExp
  | SS Type SExp
  | SFunAbs SExp Type SExp
  | SNum Integer
