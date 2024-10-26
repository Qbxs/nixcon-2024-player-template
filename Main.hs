{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import Data.Text
import Data.Time (UTCTime)
import Servant
import Servant.API

type RootAPI = Get '[JSON] ()

server :: Server RootAPI
server = return ()

app :: Application
app = serve rootAPI server

rootAPI :: Proxy RootAPI
rootAPI = Proxy

main :: IO ()
main = run 8080 app