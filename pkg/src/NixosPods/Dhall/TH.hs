{-# LANGUAGE OverloadedStrings #-}

module NixosPods.Dhall.TH
  ( withPackage,
  )
where

import Data.FileEmbed (makeRelativeToLocationPredicate)
import Data.Text qualified as T
import Dhall.TH (staticDhallExpression)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Syntax (addDependentFile)
import Relude

withPackage :: (Text -> Text) -> Q Exp
withPackage f = do
  packagePathMaybe <- liftIO $ lookupEnv "DHALL_PACKAGE_PATH"
  packagePath <-
    maybe
      ( makeRelativeToLocationPredicate (== "flake.nix") "dhall/package.dhall"
      )
      pure
      packagePathMaybe
  addDependentFile packagePath
  staticDhallExpression $ f ("(" <> T.pack packagePath <> ")")
