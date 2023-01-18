{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module NixosPods.Dhall
  ( baseStack
  , controllerStack
  , services
  )
where

import NixosPods.Dhall.TH1
import Relude
import qualified Data.Text as T

baseStack :: Text
baseStack = decodeUtf8 $(dhallToYamlTH (<> ".stacks.base"))

controllerStack :: ByteString
controllerStack = $(dhallToYamlTH (<> ".stacks.controller"))

services :: [Text]
services = T.pack <$> $(servicesTH)
