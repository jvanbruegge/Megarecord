{-# LANGUAGE UndecidableSuperClasses #-}
module Megarecord.Row.Internal where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (Symbol, Nat, KnownNat, type (+), natVal)
import Megarecord.Row (Row)
import Megarecord.Internal (Map(..))

class KnownNats (l :: [Nat]) where
    natVals :: Proxy l -> [Int]
instance KnownNats '[] where
    natVals _ = []
instance (KnownNat x, KnownNats xs) => KnownNats (x ': xs) where
    natVals _ = fromIntegral (natVal (Proxy @x)) : natVals (Proxy @xs)

type family RowIndexInternal (l :: Symbol) (r :: Row k1) (n :: Nat) :: Maybe Nat where
    RowIndexInternal l ('Cons l _ _) n = 'Just n
    RowIndexInternal l ('Cons _ _ r) n = RowIndexInternal l r (n + 1)
    RowIndexInternal _ 'Nil _ = 'Nothing

class KnownNat n => RowIndex (l :: Symbol) (r :: Row k) (n :: Nat) | l r -> n
instance (KnownNat n, RowIndexInternal l r 0 ~ 'Just n) => RowIndex l r n

class (KnownNats idx) => RowIndices (r1 :: Row k) (r2 :: Row k) (idx :: [Nat]) | r1 r2 -> idx
instance (CountIndices r1 r2 0 ~ idx, KnownNats idx) => RowIndices r1 r2 idx

class KnownNat n => RowLength (r :: Row k) (n :: Nat) | r -> n
instance (Length r ~ n, KnownNat n) => RowLength r n

type family Length (r :: Row k) :: Nat where
    Length 'Nil = 0
    Length ('Cons _ _ m) = 1 + Length m

type family CountIndices (r1 :: Row k) (r2 :: Row k) (n :: Nat) :: [Nat] where
    CountIndices 'Nil _ _ = '[]
    CountIndices ('Cons k _ m) ('Cons k _ m') n = n ': CountIndices m m' (n + 1)
    CountIndices ('Cons k v m) ('Cons k' _ m') n = CountIndices ('Cons k v m) m' (n + 1)
