{-# LANGUAGE OverloadedLabels #-}
module Main where

import Megarecord ((&), (@.), (:=) ((:=)), rnil, merge)

main :: IO ()
main = let rec1 = #bar := "World"
                & #foo := "Hello"
                & #quux := "Sparta"
                & rnil
           rec2 = #foo := "Not in the merge"
                & #bar := "also not in there"
                & #fuux := "This is"
                & rnil
           rec = merge rec1 rec2
           s = rec @. #foo ++ " " ++ rec @. #bar
           s2 = rec @. #fuux ++ " " ++ rec @. #quux
        in putStrLn s >> putStrLn s2
