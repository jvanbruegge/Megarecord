module Main where

import Data.Proxy (Proxy(..))
import Data.Kind (Type)
import Megarecord (Row, RowAppend, RowCons, Empty, Record)

type TestRow1 = RowAppend "foo" String Empty
type TestRow2 = RowAppend "bar" String TestRow1
type TestRow3 = RowAppend "fuux" Double TestRow2
type TestRow4 = RowAppend "foo" Int TestRow3

get :: forall s ty t r. RowCons s ty t r => Proxy r -> Proxy s -> ty
get _ _ = undefined

append :: forall s ty r r'. RowCons s ty r r' => Proxy r -> Proxy s -> ty -> Record r'
append _ _ _ = undefined

-- functions with _inf test type inference of the result

test_rec :: String
test_rec = get (Proxy @TestRow4) (Proxy @"foo")
--test_rec_inf = get (Record :: Record TestRow4) (Proxy :: Proxy "foo")

test_app :: Record TestRow3
test_app = append (Proxy @TestRow2) (Proxy @"fuux") (5 :: Double)
--test_app_inf = append (Record :: Record TestRow2) (Proxy :: Proxy "fuux") (5 :: Double)

main :: IO ()
main = putStrLn "hello world"
