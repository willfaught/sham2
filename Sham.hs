module Sham where

import Syntax
import Context

--hExpType :: HContext -> HExp -> SType
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

okType :: SType -> Bool
okType t = go sempty t
  where go c Lump = True
        go c Nat = True
        go c (TyVar x) = sbound x c
        go c (Label x) = go c x
        go c (Fun x y) = go c x && go c y
        go c (Forall x y) = go (stbind x c) y
        go c (Ext _ x) = and (map (go c) x)

hType :: SContext -> HExp -> Maybe SType
hType c (HAdd x y) = do xt <- hType c x
                        yt <- hType c y
                        if xt == Nat && yt == Nat then return Nat else Nothing
hType c (HApp x y) = do r <- hType c x
                        d <- hType c y
                        case r of
                          Fun p b -> if p == d then Just b else Nothing
                          _ -> Nothing
hType c (HFix x) = do t <- hType c x
                      case t of
                        Fun p b -> Just p
                        _ -> Nothing
hType c (HFunAbs v p e) = if okType p
                          then do b <- hType (sebind v p c) e
                                  return (Fun p b)
                          else Nothing
hType c (HIf0 g t f) = do gt <- hType c g
                          tt <- hType c t
                          ft <- hType c f
                          if gt == Nat && tt == ft then Just tt else Nothing
hType _ (HNum _) = Just Nat
