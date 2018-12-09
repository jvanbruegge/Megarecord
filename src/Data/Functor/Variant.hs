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

module Data.Functor.Variant (
        VariantF(..),
        inj, prj, case_, on
    ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Control.Applicative (Alternative, empty)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)

import Data.Kind.Row (Row, Empty, RowCons, FldProxy)

data VariantF (r :: Row (Type -> Type)) (a :: Type) =
    forall f. Functor f => VariantF String (f a)

instance Functor (VariantF r) where
    fmap f (VariantF l x) = VariantF l (fmap f x)

inj :: forall s f a r1 r2.
       RowCons s f r1 r2 =>
       KnownSymbol s =>
       Functor f =>
       FldProxy s -> f a -> VariantF r2 a
inj _ = VariantF (symbolVal $ Proxy @s)

prj :: forall s f a r1 r2 g.
       RowCons s f r1 r2 =>
       Alternative g =>
       KnownSymbol s =>
       FldProxy s -> VariantF r2 a -> g (f a)
prj p = on p pure (const empty)

on :: forall s f a b r1 r2.
      RowCons s f r1 r2 =>
      KnownSymbol s =>
      FldProxy s -> (f a -> b) -> (VariantF r1 a -> b) -> VariantF r2 a -> b
on _ f g v@(VariantF l x) = if symbolVal (Proxy @s) == l
                          then f (unsafeCoerce x)
                          else g (unsafeCoerce v)

case_ :: forall a b. VariantF Empty a -> b
case_ (VariantF l _) = error $ "Data.Functor.Variant: pattern match failure [\"" ++ l ++ "\"]"
