{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Variant.Internal where

import Data.Kind (Type)

import Data.Kind.Row (Row, Empty, RowCons)
import Data.Kind.RowList (RowToList, RowList(..))

class VariantMatchCases (rl :: RowList Type) (r :: Row Type) (b :: Type) | rl -> r b

instance (
        VariantMatchCases rl r' b,
        RowCons s a r' r,
        k ~ (a -> b)
    ) => VariantMatchCases ('RCons s k rl) r b

-- needed because of functional dependency (RNil can't leave b polymorphic)
instance {-# OVERLAPPING #-} (
        RowToList r ('RCons s k 'RNil),
        RowCons s a r' r,
        k ~ (a -> b)
    ) => VariantMatchCases ('RCons s k 'RNil) r b

instance VariantMatchCases 'RNil Empty ()
