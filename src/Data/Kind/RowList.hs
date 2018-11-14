{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Kind.RowList (
    RowList(..), RowToList
) where

import GHC.TypeLits (Symbol)

import Data.Kind.Row (Row)
import Data.Kind.Row.Internal(Map(..), RowPrepend)

data RowList k = RCons Symbol k (RowList k) | RNil

class RowToList (r :: Row k) (list :: RowList k) | r -> list, list -> r

instance (FromRowToList r ~ list, FromListToRow list ~ r) => RowToList r list

type family FromRowToList (r :: Row k) :: RowList k where
    FromRowToList 'Nil = 'RNil
    FromRowToList ('Cons k '[v] m) = 'RCons k v (FromRowToList m)
    FromRowToList ('Cons k (v ': xs) m) = 'RCons k v (FromRowToList ('Cons k xs m))

type family FromListToRow (l :: RowList k) :: Row k where
    FromListToRow 'RNil = 'Nil
    FromListToRow ('RCons k v m) = RowPrepend k v (FromListToRow m)
