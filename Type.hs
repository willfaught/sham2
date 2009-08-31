module Type (
  TyRule(..),
  hType,
  mType,
  sType)
  where

import Context
import Control.Monad.State.Lazy
import Syntax

newtype TyRule t = TyRule { runTyRule :: [TyDef] -> SContext -> Maybe t }

instance Monad TyRule where
  return x = TyRule (\ _ _ -> Just x)
  TyRule f >>= g =
    TyRule $ \ d c -> do t <- f d c
                         let TyRule h = g t
                         h d c
  fail _ = TyRule (\ _ _ -> Nothing)

getDefs :: TyRule [TyDef]
getDefs = TyRule (\ d _ -> Just d)

getContext :: TyRule SContext
getContext = TyRule (\ _ c -> Just c)

{-expectFunAbs :: HExp -> TyRule SType
expectFunAbs x = do getType return x
expectFunAbs _ = fail "Not HFun"
-}
getType :: HExp -> TyRule SType
getType (HNum x) = return Nat
getType (HFunAbs v t b) = do 

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

okType :: SType -> Bool
okType t = go sempty t
  where go :: SContext -> SType -> Bool
        go c Lump = True
        go c Nat = True
        go c (TyVar x) = sbound x c
        go c (Label x) = go c x
        go c (Fun x y) = go c x && go c y
        go c (Forall x y) = go (stbind x c) y
        go c (Ext _ x) = and (map (go c) x)

maybeExt :: SType -> Maybe SType
maybeExt x @ (Ext _ _) = Just x
maybeExt _ = Nothing

maybeForall :: SType -> Maybe SType
maybeForall x @ (Forall _ _) = Just x
maybeForall _ = Nothing

maybeFun :: SType -> Maybe SType
maybeFun x @ (Fun _ _) = Just x
maybeFun _ = Nothing

maybeLump :: SType -> Maybe SType
maybeLump Lump = Just Lump
maybeLump _ = Nothing

maybeNat :: SType -> Maybe SType
maybeNat Nat = Just Nat
maybeNat _ = Nothing

--hExpectExt :: [TyDef] -> SContext -> HExp -> Maybe SType
--hExpectExt x = hType


hType :: [TyDef] -> SContext -> HExp -> Maybe SType
hType d c (HAdd x y) =
  do xt <- hType d c x
     yt <- hType d c y
     if xt == Nat && yt == Nat then return Nat else Nothing
--hType d c (HCon n f) = 
hType d c (HFix x) =
  do t <- hType d c x
     case t of
       Fun p b -> Just p
       _ -> Nothing
hType d c (HFunAbs v p e) = if okType p
                            then do b <- hType d (sebind v p c) e
                                    return (Fun p b)
                            else Nothing
hType d c (HFunApp x y) = do opr <- hType d c x
                             opd <- hType d c y
                             case opr of
                               Fun p b -> if p == opd then Just b else Nothing
                               _ -> Nothing
--hType d c (HField ...
hType d c (HIf0 g t f) = do gt <- hType d c g
                            tt <- hType d c t
                            ft <- hType d c f
                            if gt == Nat && tt == ft then Just tt else Nothing
hType d c (HM t m) = do u <- mType d c m
                        if t == u then Just t else Nothing
hType _ _ (HNum _) = Just Nat
-- ...

mType :: [TyDef] -> SContext -> MExp -> Maybe SType
mType _ _ = undefined

sType :: [TyDef] -> DContext -> SExp -> Maybe DType
sType _ _ = undefined
