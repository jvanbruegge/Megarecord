{-# LANGUAGE OverloadedLabels #-}
module Main where

import Megarecord ((&), (:=) ((:=)), get, rnil)

main :: IO ()
main = let rec = #bar := "World"
               & #foo := "Hello"
               & rnil
           s = get #foo rec ++ " " ++ get #bar rec
        in putStrLn s
