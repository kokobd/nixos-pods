module NixosPods.Dhall.TH1
  ( dhallToYamlTH,
    servicesTH,
  )
where

import Control.Lens
import Data.Char (isUpper, toLower)
import Data.FileEmbed (bsToExp)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Dhall qualified
import Dhall.JSON (omitNull)
import Dhall.JSON.Yaml (Options (omission), defaultOptions, dhallToYaml)
import Language.Haskell.TH
import NixosPods.Dhall.TH0
import Relude

dhallPackagePath :: Text
dhallPackagePath = T.pack $(dhallPackagePathTH)

dhallToYamlTH :: (Text -> Text) -> Q Exp
dhallToYamlTH f = bsToExp =<< liftIO (dhallToYaml opts Nothing (f [i|(#{dhallPackagePath})|]))
  where
    opts = defaultOptions {omission = omitNull}

servicesTH :: Q Exp
servicesTH = do
  strs <- liftIO $ Dhall.input (Dhall.list Dhall.string) [i|(#{dhallPackagePath}).services|]
  pure . ListE . fmap (LitE . StringL . toExecutableName) $ strs

toExecutableName :: String -> String
toExecutableName name =
  concatMap
    ( \x ->
        if isUpper x
          then ['-', toLower x]
          else [x]
    )
    (name & _head %~ toLower)
