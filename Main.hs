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
-- import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
-- import System.Directory
-- import Text.Blaze
-- import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
-- import qualified Data.Aeson.Parser
-- import qualified Text.Blaze.Html

type RootAPI = Get '[JSON] ()
--   :<|> "add" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Int

server :: Server RootAPI
server = return ()
--   <|> return (\x y -> x + y)

app :: Application
app = serve rootAPI server

rootAPI :: Proxy RootAPI
rootAPI = Proxy

main :: IO ()
main = run 8080 app