{-# LANGUAGE TemplateHaskell #-}

module NixosPods.Dhall
  ( base,
  )
where

import NixosPods.Dhall.TH (dhallExpr)
import Dhall.Core (Expr)

base :: Expr s a
base = $(dhallExpr)
