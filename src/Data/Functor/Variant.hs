{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Functor.Variant (
        VariantF, VariantC, VariantCS(..),
        inj, prj, case_, on
    ) where

import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy(..))
import Control.Applicative (Alternative, empty)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)

import Debug.Trace

import Data.Kind.Row (Row, Empty, RowCons, FldProxy)

data VariantCS (cs :: [(k -> Type) -> Constraint]) (r :: Row (k -> Type)) (a :: k) =
        forall f. All cs f => VariantCS String (f a)

type VariantC c r a = VariantCS '[c] r a
type VariantF r a = VariantC Functor r a

type family All (cs :: [k -> Constraint]) (f :: k) :: Constraint where
    All '[] _ = ()
    All (c ': cs) f = (c f, All cs f)

instance Functor (VariantCS (Functor ': cs) r) where
    fmap f (VariantCS l x) = VariantCS l (fmap f x)

inj :: forall cs s f a r1 r2.
       RowCons s f r1 r2 =>
       KnownSymbol s =>
       All cs f =>
       FldProxy s -> f a -> VariantCS cs r2 a
inj _ = VariantCS (symbolVal $ Proxy @s)

prj :: forall c s f a r1 r2 g.
       RowCons s f r1 r2 =>
       Alternative g =>
       KnownSymbol s =>
       FldProxy s -> VariantCS c r2 a -> g (f a)
prj p = on p pure (const empty)

on :: forall cs s f a b r1 r2.
      RowCons s f r1 r2 =>
      KnownSymbol s =>
      FldProxy s -> (f a -> b) -> (VariantCS cs r1 a -> b) -> VariantCS cs r2 a -> b
on _ f g v@(VariantCS l x) = if symbolVal (Proxy @s) == l
                          then f (unsafeCoerce x)
                          else g (unsafeCoerce v)

case_ :: forall cs a b. VariantCS cs Empty a -> b
case_ (VariantCS l _) = error $ "Data.Functor.Variant: pattern match failure [\"" ++ l ++ "\"]"
