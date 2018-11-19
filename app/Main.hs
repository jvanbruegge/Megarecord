{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Kind.Row (type (&), type (:::), Empty)
import Data.Record (Record, insert, rnil)
import Data.Variant (Variant, inj, case_, on)

rec2 :: Record ("foo" ::: (String -> String) & Empty)
rec2 = insert #foo (\foo -> "foo" ++ foo) rnil

type MyVariant = Variant ("foo" ::: String & "bar" ::: Int & "quux" ::: Float & Empty)

v :: [MyVariant]
v = [
        inj #foo "bar",
        inj #bar 7,
        inj #quux 4.3
    ]

(&) :: a -> (a -> b) -> b
(&) = flip ($)

f :: MyVariant -> String
f = case_
    & on #foo id
    & on #bar (show . (*2))
    & on #quux show

main :: IO ()
main = mapM_ (putStrLn . f) v
