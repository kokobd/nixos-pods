{-# LANGUAGE TemplateHaskell #-}

module Embed (baseDhall, controllerDhall) where

import Data.FileEmbed (makeRelativeToProject)
import Data.Text qualified as T
import Dhall.Core (Expr)
import Dhall.Src (Src)
import Dhall.TH (staticDhallExpression)
import Relude

baseDhall :: Expr Src Void
baseDhall = $(makeRelativeToProject "exe/deploy/base.dhall" >>= staticDhallExpression . T.pack)

controllerDhall :: Expr Src Void
controllerDhall = $(makeRelativeToProject "exe/deploy/controller.dhall" >>= staticDhallExpression . T.pack)
