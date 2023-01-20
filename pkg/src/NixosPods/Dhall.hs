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
import Control.Lens

baseStack :: Text
baseStack = decodeUtf8 $(dhallToYamlTH (<> ".stacks.base"))

controllerStack :: Text
controllerStack = decodeUtf8 $(dhallToYamlTH (<> ".stacks.controller"))

-- | from image logical names listed in dhall (LikeThis) to executables listed in cabal files (like-this)
services :: Map Text Text
services = fromList $ $(servicesTH) & traverse . both %~ T.pack
