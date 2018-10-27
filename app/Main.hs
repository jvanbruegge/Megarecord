{-# LANGUAGE OverloadedLabels #-}
module Main where

import Megarecord (RowAppend, Empty, {-Record,-} insert, get, rnil)

type TestRow1 = RowAppend "foo" String Empty
type TestRow2 = RowAppend "bar" String TestRow1
type TestRow3 = RowAppend "fuux" Double TestRow2
type TestRow4 = RowAppend "foo" Int TestRow3

main :: IO ()
main = let emptyRec = rnil
           rec1 = insert #foo "Hello" emptyRec
           rec2 = insert #bar "World" rec1
           s = get #foo rec2 ++ " " ++ get #bar rec2
        in putStrLn s
