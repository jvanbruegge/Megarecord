{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Record.Helpers where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownNat, Nat, natVal, Symbol, type (+))

import Data.Kind.Row (RowCons)
import Data.Kind.RowList (RowToList, RowList(..))

class (RowCons s ty r' r) => RecordCons s ty r' r where
    getIndex :: Proxy s -> Proxy r -> Int

instance (
        RowCons s ty r' r,
        RowToList r rl,
        RowListIndex s rl ~ n,
        KnownNat n
    ) => RecordCons s ty r' r where
        getIndex _ _ = fromIntegral $ natVal (Proxy @n)

type family RowListIndex (s :: Symbol) (rl :: RowList k) :: Nat where
    RowListIndex s ('RCons s _ _) = 0
    RowListIndex s ('RCons k _ m) = 1 + RowListIndex s m
