{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Prelude ()
import "base-compat" Prelude.Compat

import Control.Monad.IO.Class
import Servant
import Network.Wai.Handler.Warp (run)
import System.Process (readProcess)
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)


type API = Get '[PlainText] String
     :<|> "add" :> Capture "x" Int :> Capture "y" Int :> Get '[PlainText] String
     :<|> "mul" :> Capture "x" Int :> Capture "y" Int :> Get '[PlainText] String
     :<|> "cowsay" :> Capture "string" String :> Get '[PlainText] String
     :<|> "uuid" :> Get '[PlainText] String

server :: Server API
server = base
    :<|> add
    :<|> mul
    :<|> cowsay 
    :<|> uuid

base :: Handler String 
base = return ""

add :: Int -> Int -> Handler String
add x y = return $ show $ x + y

mul :: Int -> Int -> Handler String
mul x y = return $ show $ x * y

uuid :: Handler String
uuid = toString <$> liftIO nextRandom

cowsay :: String -> Handler String
cowsay s = liftIO $ readProcess "cowsay" [s] ""

rootAPI :: Proxy API
rootAPI = Proxy

app :: Application
app = serve rootAPI server

main :: IO ()
main = run 8080 app