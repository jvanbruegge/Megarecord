{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O2 -fplugin Test.Inspection.Plugin #-}
{-# OPTIONS_GHC -dsuppress-coercions #-}
{-# OPTIONS_GHC -dsuppress-module-prefixes #-}

module Main where

import Test.Inspection
import Data.Record

s, s1 :: String
s = show (#foo := "foo" & #bar := (7 :: Int) & #quux := Just False & rnil)
s1 = "{ bar: 7, foo: \"foo\", quux: Just False }"

inspect $ 's === 's1

main :: IO ()
main = pure ()
