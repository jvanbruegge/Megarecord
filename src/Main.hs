{-# LANGUAGE ConstraintKinds #-}
module Main where

import Data.Proxy (Proxy(..))
import Data.Kind (Type, Constraint)
import GHC.TypeLits
import Fcf (Exp, Eval, FromMaybe, ConstFn, TyEq, Filter, Not, Null, Flip, Head, type (<=<), type (=<<), type (++), Pure)
import qualified Fcf as F
import Megarecord (Row, RemoveWith, InsertWith, Lookup, Empty, Map, Traverse)

data FromJust :: Maybe k -> Exp k
type instance Eval (FromJust ('Just k)) = k

data FormatRowLabel :: Symbol -> [k2] -> Exp ErrorMessage
type instance Eval (FormatRowLabel k '[]) = 'Text ""
type instance Eval (FormatRowLabel k (x ': '[])) = 'Text k ':<>: 'Text " :: " ':<>: 'ShowType x
type instance Eval (FormatRowLabel k (x ': x' ': xs)) = 'Text k ':<>: 'Text " :: " ':<>: 'ShowType x ':<>: 'Text ", " ':<>: Eval (FormatRowLabel k (x' ': xs))

type family PrettyPrintRow (r :: Row k1) :: ErrorMessage where
    PrettyPrintRow r = 'Text "(" ':<>: Traverse FormatRowLabel ConcatRow r ('Text "()") ':<>: 'Text ")"

data ConcatRow :: ErrorMessage -> ErrorMessage -> Exp ErrorMessage
type instance Eval (ConcatRow a b) = a ':<>: 'Text ", " ':<>: b

data Concat :: a -> a -> Exp a
type instance Eval (Concat (s :: Symbol) (s' :: Symbol)) = AppendSymbol s s'
type instance Eval (Concat (t :: ErrorMessage) (t' :: ErrorMessage)) = t ':<>: t'

type Insert (k :: k1) (v :: k2) (m :: Map k1 k2) = Eval (InsertWith (Flip ConstFn) k v m)

type RowDelete (s :: Symbol) (r :: Row k1) = Eval (RemoveWith RowDeleteInternal s r)
data RowDeleteInternal :: [k] -> Exp (Maybe [k2])
type instance Eval (RowDeleteInternal '[x]) = 'Nothing
type instance Eval (RowDeleteInternal (x ': (x' ': xs))) = 'Just (x' ': xs)

type Contains (mapping :: k2 -> Exp Bool) (k :: k1) (m :: Map k1 k2) =
    Eval (FromMaybe 'False =<< F.Map mapping =<< Pure (Lookup k m))

type ContainsKey (k :: k1) (m :: Map k1 k2) = Contains (ConstFn 'True) k m
type ContainsKeyValue (k :: k1) (v :: k2) (m :: Map k1 k2) = Contains (TyEq v) k m

type RowContains (k :: Symbol) (v :: k2) (m :: Row k2) =
    Contains (Not <=< Null <=< Filter (TyEq v)) k m

type RecordContains (k :: Symbol) (v :: Type) (m :: Row Type) =
    Contains (FromMaybe 'False <=< F.Map (TyEq v) <=< Head) k m

type Has (s :: Symbol) (ty :: k2) (r :: Row k2) = Has_ s ty r (RowContains s ty r) 'False
type RecordHas (s :: Symbol) (ty :: Type) (r :: Row Type) = Has_ s ty r (RecordContains s ty r) 'True
type family Has_ (s :: Symbol) (ty :: k2) (r :: Row k2) (result :: Bool) (isRecord :: Bool) :: Constraint where
    Has_ s ty r 'False 'False = TypeError (
        'Text "(" ':<>: 'Text s ':<>: 'Text " :: " ':<>: 'ShowType ty
        ':<>: 'Text ") is not part of the Row " ':<>: 'ShowType r)
    Has_ s ty r 'False 'True = TypeError (
        'Text "(" ':<>: 'Text s ':<>: 'Text " :: " ':<>: 'ShowType ty
        ':<>: 'Text ") is not the first occurence of the label in the Row "
        ':<>: PrettyPrintRow r)
    Has_ _ _ _ 'True _ = ()

type NotHas (s :: Symbol) (ty :: k2) (r :: Row k2) = NotHas_ s ty r (RowContains s ty r)
type family NotHas_ (s :: Symbol) (ty :: k2) (r :: Row k2) (result :: Bool) :: Constraint where
    NotHas_ s ty r 'True = TypeError (
        'Text "(" ':<>: 'Text s ':<>: 'Text " :: " ':<>: 'ShowType ty
        ':<>: 'Text ") is already part of the Row " ':<>: PrettyPrintRow r)
    NotHas_ _ _ _ 'False = ()


class RowCons (label :: Symbol) (ty :: k) (tail :: Row k) (row :: Row k) | label row -> ty tail, label ty tail -> row
instance (RowDelete s r ~ tail, RowPrepend s ty tail ~ r) => RowCons s ty tail r

-----

type RowInsert (k :: Symbol) (v :: k2) (m :: Row k2) = Eval (InsertWith RowAdd k '[v] m)
type RowPrepend (k :: Symbol) (v :: k2) (m :: Row k2) = Eval (InsertWith RowPrep_ k '[v] m)

data RowPrep_ :: Maybe [v] -> [v] -> Exp [v]
type instance Eval (RowPrep_ 'Nothing v) = v
type instance Eval (RowPrep_ ('Just xs) '[v]) = v ': xs

data RowAdd :: Maybe [v] -> [v] -> Exp [v]
type instance Eval (RowAdd 'Nothing v) = v
type instance Eval (RowAdd ('Just xs) '[v]) = Eval (xs ++ '[v])

-----

type TestRow1 = RowInsert "foo" String Empty
type TestRow2 = RowInsert "bar" String TestRow1
type TestRow3 = RowInsert "fuux" Double TestRow2
type TestRow4 = RowInsert "foo" Int TestRow3

data Record (r :: Row Type) = Record

---- Tests -----------
get :: forall s ty t r. RowCons s ty t r => Record r -> Proxy s -> ty
get _ _ = undefined

append :: forall s ty r r'. RowCons s ty r r' => Record r -> Proxy s -> ty -> Record r'
append _ _ _ = undefined

test_rec :: String
test_rec = get (Record :: Record TestRow4) (Proxy :: Proxy "foo")
test_rec_inf = get (Record :: Record TestRow4) (Proxy :: Proxy "foo")

test_app :: Record TestRow3
test_app = append (Record :: Record TestRow2) (Proxy :: Proxy "fuux") (5 :: Double)
test_app_inf = append (Record :: Record TestRow2) (Proxy :: Proxy "fuux") (5 :: Double)

main :: IO ()
main = putStrLn "hello world"
