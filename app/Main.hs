{-# LANGUAGE OverloadedLabels, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Main where

import Servant
import Data.Aeson (FromJSON, ToJSON)
import Network.Wai.Handler.Warp (run)

import Megarecord ((&), type (&), (:=) ((:=)), rnil, Record, RowDelete, Empty, insert)

newtype Id = Id Int deriving (Show, FromJSON, ToJSON)

type User = Record ("id" := Id & "name" := String & "age" := Int & Empty)

testUser :: User
testUser = #id := Id 8
     & #name := "Simon"
     & #age := 7
     & rnil

type family WithoutId r where
    WithoutId (Record r) = Record (RowDelete "id" r)

type API = "user" :> ReqBody '[JSON] (WithoutId User) :> Post '[JSON] User

apiHandler :: Server API
apiHandler rec = pure $ insert #id (Id 0) rec

main :: IO ()
main = run 8080 $ serve (Proxy @API) apiHandler
