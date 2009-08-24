module Context (
  DContext(),
  SContext(),
  dbind,
  dbinding,
  dempty,
  sebind,
  sempty,
  sbinding,
  sbound,
  stbind)
  where

import Syntax

newtype DContext =
  DContext
    [(EVar, DType)]
  deriving (Eq, Show)

dbind :: EVar -> DType -> DContext -> DContext
dbind v t (DContext b) = DContext ((v, t) : b)

dbinding :: EVar -> DContext -> Maybe DType
dbinding v (DContext b) = lookup v b

dempty = DContext []

data SContext =
  SContext
    [(EVar, SType)]
    [TVar]
  deriving (Eq, Show)

sebind :: EVar -> SType -> SContext -> SContext
sebind v t (SContext es ts) = SContext ((v, t) : es) ts

sempty = SContext [] []

sbinding :: EVar -> SContext -> Maybe SType
sbinding v (SContext e _) = lookup v e

sbound :: TVar -> SContext -> Bool
sbound v (SContext _ t) = v `elem` t

stbind :: TVar -> SContext -> SContext
stbind t (SContext es ts) = SContext es (t : ts)
