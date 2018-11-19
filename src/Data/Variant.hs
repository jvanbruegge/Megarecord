{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module Data.Variant (
    Variant,
    inj, prj, on, case_, expand
) where

import Data.Kind (Type)
import Control.Applicative (Alternative, empty)
import GHC.Base (Any)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)

import Data.Kind.Row (Row, Empty, RowCons, RowUnion, FldProxy(..))
{-import Data.Kind.RowList (RowToList)
import Data.Record (Record)
import Data.Variant.Internal (VariantMatchCases)-}

data Variant (r :: Row Type) = Variant String Any

inj :: forall s a r1 r2.
       RowCons s a r1 r2 =>
       KnownSymbol s =>
       FldProxy s -> a -> Variant r2
inj p value = Variant (symbolVal p) (unsafeCoerce value)

prj :: forall s a r1 r2 f.
       RowCons s a r1 r2 =>
       KnownSymbol s =>
       Alternative f =>
       FldProxy s -> Variant r2 -> f a
prj p = on p pure (const empty)

on :: forall s a b r1 r2.
      RowCons s a r1 r2 =>
      KnownSymbol s =>
      FldProxy s -> (a -> b) -> (Variant r1 -> b) -> Variant r2 -> b
on p f g v@(Variant l val) = if symbolVal p == l
                           then f (unsafeCoerce val)
                           else g (unsafeCoerce v)

case_ :: forall a. Variant Empty -> a
case_ (Variant s _) = error $ "Data.Variant: pattern match failure [\"" ++ s ++ "\"]"

{-onMatch :: forall rl r r1 r2 r3 b.
           RowToList r rl =>
           VariantMatchCases rl r1 b =>
           RowUnion r1 r2 r3 =>
           KnownLabels r =>
           Record r -> (Variant r2 -> b) -> Variant r3 -> b
onMatch r k v@(Variant l val) = case maybeIndex l (getLabels $ Proxy @r) of
                              Just i -> (unsafeCoerce @_ @(Va $ unsafeGet i
-}

expand :: forall lt a gt.
          RowUnion lt a gt =>
          Variant lt -> Variant gt
expand = unsafeCoerce
