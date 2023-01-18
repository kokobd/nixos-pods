{-# LANGUAGE OverloadedStrings #-}

module NixosPods.Dhall.TH0
  ( dhallPackagePathTH
  )
where

import Data.FileEmbed (makeRelativeToLocationPredicate)
import Language.Haskell.TH (Exp (..), Lit (..), Q)
import Language.Haskell.TH.Syntax (addDependentFile)
import Relude

dhallPackagePathTH :: Q Exp
dhallPackagePathTH = do
  packagePathMaybe <- liftIO $ lookupEnv "DHALL_PACKAGE_PATH"
  packagePath <-
    maybe
      ( makeRelativeToLocationPredicate (== "flake.nix") "dhall/package.dhall"
      )
      pure
      packagePathMaybe
  addDependentFile packagePath
  pure . LitE . StringL $ packagePath
