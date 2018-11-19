{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}

module Data.Kind.Row.Utils (
        KnownLabels, getLabels
    ) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)

import Data.Kind.Row (Row)
import Data.Kind.RowList (RowToList, RowList(..))

class KnownLabels (r :: Row k) where
    getLabels :: Proxy r -> [String]

instance (RowToList r rl, RowListLabels rl) => KnownLabels r where
    getLabels _ = getRowListLabels $ Proxy @rl

class RowListLabels (rl :: RowList k) where
    getRowListLabels :: Proxy rl -> [String]

instance RowListLabels 'RNil where
    getRowListLabels _ = []

instance (KnownSymbol k, RowListLabels rl) => RowListLabels ('RCons k v rl) where
    getRowListLabels _ = symbolVal (Proxy @k) : getRowListLabels (Proxy @rl)
