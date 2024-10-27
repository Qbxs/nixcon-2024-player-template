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


type API = Get '[JSON] String
     :<|> "add" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] (APIResult Int)
     :<|> "mul" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] (APIResult Int)
     :<|> "cowsay" :> Capture "string" String :> Get '[JSON] (APIResult String)
     :<|> "uuid" :> Get '[JSON] (APIResult String)

newtype APIResult a = APIResult { result :: a }
  deriving Generic
instance (ToJSON a) => ToJSON (APIResult a)

server :: Server API
server = base
    :<|> add
    :<|> mul
    :<|> cowsay 
    :<|> uuid

base :: Handler String
base = return ""

add :: Int -> Int -> Handler (APIResult Int)
add x y = return $ APIResult $ x + y

mul :: Int -> Int -> Handler (APIResult Int)
mul x y = return $ APIResult $ x * y

uuid :: Handler (APIResult String)
uuid = do
  uuid <- liftIO $ nextRandom
  return $ APIResult $ toString uuid

cowsay :: String -> Handler (APIResult String)
cowsay s = do
    moo <- liftIO $ readProcess "cowsay" [s] ""
    return $ APIResult moo

rootAPI :: Proxy API
rootAPI = Proxy

app :: Application
app = serve rootAPI server

main :: IO ()
main = run 8080 app