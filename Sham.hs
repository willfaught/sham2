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

expect :: SType -> Maybe SType -> Maybe SType
expect e Nothing = Nothing
expect e (Just a) = if e == a then Just a else Nothing

hType :: SContext -> HExp -> Maybe SType
hType c 
hType c (HAdd x y) = do xt <- hType c x
                        yt <- hType c y
                        if xt == Nat && yt == Nat then return Nat else Nothing
