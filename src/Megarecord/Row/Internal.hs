module Megarecord.Row.Internal where

import GHC.TypeLits (Symbol, Nat, KnownNat, type (+))
import Megarecord.Row (Row)
import Megarecord.Internal (Map(..))

type family RowIndexInternal (l :: Symbol) (r :: Row k1) (n :: Nat) :: Maybe Nat where
    RowIndexInternal l ('Cons l _ _) n = 'Just n
    RowIndexInternal l ('Cons _ _ r) n = RowIndexInternal l r (n + 1)
    RowIndexInternal _ 'Nil _ = 'Nothing

class KnownNat n => RowIndex l r n | l r -> n
instance (KnownNat n, RowIndexInternal l r 0 ~ 'Just n) => RowIndex l r n
