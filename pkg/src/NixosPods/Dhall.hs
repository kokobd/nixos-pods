{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module NixosPods.Dhall
  ( base,
  )
where

import Data.String.Interpolate (i)
import Dhall.Core (Expr)
import NixosPods.Dhall.TH (withPackage)

base :: Expr s a
base = $(withPackage (\pkg -> [i|#{pkg}.base|]))
