module Megarecord.Row.Internal where

import GHC.TypeLits (Symbol, Nat, KnownNat, type (+))
import Megarecord.Row (Row)
import Megarecord.Internal (Map(..), Compare)

type family RowIndexInternal (l :: Symbol) (r :: Row k1) (n :: Nat) :: Maybe Nat where
    RowIndexInternal l ('Cons l _ _) n = 'Just n
    RowIndexInternal l ('Cons _ _ r) n = RowIndexInternal l r (n + 1)
    RowIndexInternal _ 'Nil _ = 'Nothing

class KnownNat n => RowIndex l r n | l r -> n
instance (KnownNat n, RowIndexInternal l r 0 ~ 'Just n) => RowIndex l r n

class KnownNat n => RowInsertIndex l r n | l r -> n
instance (KnownNat n, RowInsertIndex_ l r 0 ~ n) => RowInsertIndex l r n

type family RowInsertIndex_ (l :: Symbol) (r :: Row k1) (n :: Nat) :: Nat where
    RowInsertIndex_ l 'Nil n = n
    RowInsertIndex_ l ('Cons s _ r) n = RowInsertIndex__ (Compare l s) l r n

type family RowInsertIndex__ (o :: Ordering) (l :: Symbol) (r :: Row k1) (n :: Nat) :: Nat where
    RowInsertIndex__ 'LT _ _ n = n
    RowInsertIndex__ 'GT l 'Nil n = n + 1
    RowInsertIndex__ 'GT l ('Cons _ _ r) n = RowInsertIndex_ l r (n + 1)
