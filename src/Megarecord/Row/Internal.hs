{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Megarecord.Row.Internal where

import Data.Aeson (Value, ToJSON(..))
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.Base (Any, Int(..))
import GHC.Prim
import GHC.TypeLits (Symbol, Nat, KnownNat, KnownSymbol, type (+), natVal, symbolVal)
import Unsafe.Coerce (unsafeCoerce)

import Megarecord.Internal (Map(..), Row)

class KnownNats (l :: [Nat]) where
    natVals :: Proxy l -> [Int]
instance KnownNats '[] where
    natVals _ = []
instance (KnownNat x, KnownNats xs) => KnownNats (x ': xs) where
    natVals _ = fromIntegral (natVal (Proxy @x)) : natVals (Proxy @xs)

class KnownLabels (r :: Row k) where
    getLabels :: Proxy r -> [String]
instance KnownLabels 'Nil where
    getLabels _ = []
instance (KnownSymbol s, KnownLabels m) => KnownLabels ('Cons s v m) where
    getLabels _ = symbolVal (Proxy @s) : getLabels (Proxy @m)

class ValuesToJSON (r :: Row Type) where
    toValues :: Proxy r -> Int -> SmallArray# Any -> [Value]
instance ValuesToJSON 'Nil where
    toValues _ _ _ = []
instance (ValuesToJSON m, ToJSON v) => ValuesToJSON ('Cons k '[v] m) where
    toValues :: Proxy p -> Int -> SmallArray# Any -> [Value]
    toValues _ n a# = toJSON (unsafeCoerce @Any @v val) : toValues (Proxy @m) (n + 1) a#
        where (# val #) = indexSmallArray# a# i#
              !(I# i#) = n

-- Misc
type RowIndex (l :: Symbol) (r :: Row k) = RowIndexInternal l r 0
type family RowIndexInternal (l :: Symbol) (r :: Row k1) (n :: Nat) :: Nat where
    RowIndexInternal l ('Cons l _ _) n = n
    RowIndexInternal l ('Cons _ _ r) n = RowIndexInternal l r (n + 1)

type family RowLength (r :: Row k) :: Nat where
    RowLength 'Nil = 0
    RowLength ('Cons _ _ m) = 1 + RowLength m


type RowIndices (r1 :: Row k) (r2 :: Row k) = RowIndicesInternal r1 r2 0
type family RowIndicesInternal (r1 :: Row k) (r2 :: Row k) (n :: Nat) :: [Nat] where
    RowIndicesInternal 'Nil _ _ = '[]
    RowIndicesInternal ('Cons k _ m) ('Cons k _ m') n = n ': RowIndicesInternal m m' (n + 1)
    RowIndicesInternal ('Cons k v m) ('Cons k' _ m') n = RowIndicesInternal ('Cons k v m) m' (n + 1)
