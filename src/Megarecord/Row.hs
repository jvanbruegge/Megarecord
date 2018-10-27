module Megarecord.Row (
        Row, Empty,
        RowCons, RowLacks,
        RowAppend, RowPrepend,
        RowDelete
    ) where

import GHC.TypeLits (Symbol)
import Megarecord.Internal (Map, Empty, RemoveWith, InsertWith, Lookup)
import Fcf (Eval, Exp, type (++))

type Row k = Map Symbol [k]


class RowCons (label :: Symbol) (ty :: k) (tail :: Row k) (row :: Row k)
        | label row -> ty tail, label ty tail -> row
instance (RowDelete s r ~ tail, RowPrepend s ty tail ~ r) => RowCons s ty tail r

class RowLacks (label :: Symbol) (row :: Row k)
instance (Lookup label row ~ 'Nothing) => RowLacks label row



type RowDelete (s :: Symbol) (r :: Row k1) = Eval (RemoveWith RowDeleteInternal s r)
data RowDeleteInternal :: [k] -> Exp (Maybe [k2])
type instance Eval (RowDeleteInternal '[x]) = 'Nothing
type instance Eval (RowDeleteInternal (x ': (x' ': xs))) = 'Just (x' ': xs)


type RowAppend (k :: Symbol) (v :: k2) (m :: Row k2) = Eval (InsertWith RowAdd k '[v] m)
data RowAdd :: Maybe [v] -> [v] -> Exp [v]
type instance Eval (RowAdd 'Nothing v) = v
type instance Eval (RowAdd ('Just xs) '[v]) = Eval (xs ++ '[v])


type RowPrepend (k :: Symbol) (v :: k2) (m :: Row k2) = Eval (InsertWith RowPrep k '[v] m)
data RowPrep :: Maybe [v] -> [v] -> Exp [v]
type instance Eval (RowPrep 'Nothing v) = v
type instance Eval (RowPrep ('Just xs) '[v]) = v ': xs
