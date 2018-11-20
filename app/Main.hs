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
import qualified Data.Functor.Variant as VF

rec2 :: Record ("foo" ::: (String -> String) & Empty)
rec2 = insert #foo (\foo -> "foo" ++ foo) rnil

type MyVariant = Variant ("foo" ::: String & "bar" ::: Int & "quux" ::: Float & Empty)

v :: [MyVariant]
v = [
        inj #foo "bar",
        inj #bar 7,
        inj #quux 4.3
    ]

type MyVariantF a = VF.VariantF ("maybe" ::: Maybe & "either" ::: Either String & Empty) a

vf :: [MyVariantF Int]
vf = [
        VF.inj #maybe (Just 5),
        VF.inj #either (Left "oops")
    ]

(&) :: a -> (a -> b) -> b
(&) = flip ($)

fromEither :: Either a a -> a
fromEither (Left x) = x
fromEither (Right x) = x

f :: MyVariant -> String
f = case_
    & on #foo id
    & on #bar (show . (*2))
    & on #quux show

ff :: MyVariantF Int -> String
ff = VF.case_
    & VF.on #maybe show
    & VF.on #either (fromEither . fmap show)

main :: IO ()
main = mapM_ (putStrLn . f) v *> mapM_ (putStrLn . ff . fmap (*2)) vf
