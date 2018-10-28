module Megarecord.Record.Combinators where

import Megarecord.Row (RowCons, RowLacks)
import Megarecord.Row.Internal (RowInsertIndex)
import Megarecord.Record (Record, FldProxy, insert)

data label := value = FldProxy label := !value
infix 7 :=

(&) :: forall l v r1 r2 i.
    RowLacks l r1 =>
    RowCons l v r1 r2 =>
    RowInsertIndex l r1 i =>
    (l := v) -> Record r1 -> Record r2
(&) (lp := v) = insert lp v
infixr 5 &
