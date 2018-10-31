{-# LANGUAGE OverloadedLabels, FlexibleContexts #-}
module Main where

import Megarecord ((&), (@.), (:=) ((:=)), rnil, merge, RowCons, Record, RowAppend, Empty, insert, RowLacks)

newtype Id = Id Int deriving Show

-- With sugar: rec :: { foo :: String, bar :: String, name :: String }
rec1 :: Record (RowAppend "foo" String (RowAppend "bar" String (RowAppend "quux" String Empty)))
-- With sugar: rec = { foo = "World", bar = "Hello", name = "Peter" }
rec1 = #foo := "World" & #bar := "Hello" & #quux := "Sparta" & rnil

f :: forall r r2. (RowLacks "id" r, RowCons "id" Id r r2) => Record r -> Record r2
f = insert #id (Id 0)

main :: IO ()
main = let rec1' = f rec1
           rec2 = #foo := "Not in the merge"
                & #bar := "also not in there"
                & #fuux := "This is"
                & rnil
           rec = merge rec1' rec2
           s = rec @. #foo ++ " " ++ rec @. #bar
           s2 = rec @. #fuux ++ " " ++ rec @. #quux
        in putStrLn s >> putStrLn s2 >> putStrLn (show $ rec @. #id)
