module Context (
  Context(),
  empty,
  dbind,
  sebind,
  stbind,
  dbinding,
  sbinding,
  sbound)
  where

import Syntax

data Context =
  Context {
    dynExp :: [(EVar, DType)],
    statExp :: [(EVar, SType)],
    statType :: [TVar] }
  deriving (Eq, Show)

empty = Context [] [] []

dbind :: EVar -> DType -> Context -> Context
dbind v t x @ (Context b _ _) = x { dynExp = (v, t) : b }

sebind :: EVar -> SType -> Context -> Context
sebind v t x @ (Context _ es ts) = x { statExp = (v, t) : es }

stbind :: TVar -> Context -> Context
stbind t x @ (Context _ _ ts) = x { statType = t : ts }

dbinding :: EVar -> Context -> Maybe DType
dbinding v (Context b _ _) = lookup v b

sbinding :: EVar -> Context -> Maybe SType
sbinding v (Context _ e _) = lookup v e

sbound :: TVar -> Context -> Bool
sbound v (Context _ _ t) = v `elem` t
