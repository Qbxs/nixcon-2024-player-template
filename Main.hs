{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Text
import Data.Time (UTCTime)
import Servant
import Servant.API

type RootAPI = Get '[JSON] ()
  <|> "add" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Int

server :: Server RootAPI
server = return ()
  <|> return (\x y -> x + y)

app :: Application
app = serve rootAPI server

rootAPI :: Proxy RootAPI
rootAPI = Proxy

main :: IO ()
main = run 8080 app