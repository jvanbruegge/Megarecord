module Megarecord.Record.Combinators where

import Megarecord.Row (RowCons, RowLacks)
import Megarecord.Row.Internal (RowIndex)
import Megarecord.Record (Record, FldProxy, insert, get)

data label := value = FldProxy label := !value
infix 7 :=

(&) :: forall l v r1 r2 i.
    RowLacks l r1 =>
    RowCons l v r1 r2 =>
    RowIndex l r2 i =>
    (l := v) -> Record r1 -> Record r2
(&) (lp := v) = insert lp v
infixr 5 &

(@.) :: forall l ty r r' i.
    RowCons l ty r' r =>
    RowIndex l r i =>
    Record r -> FldProxy l -> ty
(@.) r p = get p r
infix 8 @.
