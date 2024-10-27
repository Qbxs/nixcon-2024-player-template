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

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
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