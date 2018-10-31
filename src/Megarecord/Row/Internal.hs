{-# LANGUAGE UndecidableSuperClasses #-}
module Megarecord.Row.Internal where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (Symbol, Nat, KnownNat, type (+), natVal)
import Megarecord.Internal (Map(..), Row)

class KnownNats (l :: [Nat]) where
    natVals :: Proxy l -> [Int]
instance KnownNats '[] where
    natVals _ = []
instance (KnownNat x, KnownNats xs) => KnownNats (x ': xs) where
    natVals _ = fromIntegral (natVal (Proxy @x)) : natVals (Proxy @xs)

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
