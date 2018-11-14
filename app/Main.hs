{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Aeson (FromJSON, ToJSON)
import Network.Wai.Handler.Warp (run)
import Servant

import Data.Kind.Row (type (&), type (:::), Empty)
import Data.Record (Record, insert)

newtype Id = Id Int deriving (Show, FromJSON, ToJSON)
type family WithId r where
    WithId (Record r) = Record ("id" ::: Id & r)

type UserR = Record ("name" ::: String & "age" ::: Int & Empty)
type User = WithId UserR

type API = "user" :> ReqBody '[JSON] UserR :> Post '[JSON] User

apiHandler :: Server API
apiHandler rec = pure $ insert #id (Id 0) rec

main :: IO ()
main = run 8080 $ serve (Proxy @API) apiHandler
