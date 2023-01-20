{-# LANGUAGE ApplicativeDo #-}

module NixosPods.Tool.Environment
  ( EnvVarsE (..),
    askEnvVar,

    -- * Interpreters
    runEnvVarsE,
  )
where

import Effectful.Reader.Static
import Env hiding (Reader)
import NixosPods.Prelude

data EnvVarsE :: Effect where
  AskEnvVar :: (EnvVars -> a) -> EnvVarsE m a

data EnvVars = EnvVars
  { serviceImagesDir :: FilePath,
    skopeoPath :: FilePath
  }
  deriving stock (Show, Eq, Generic)

makeEffect ''EnvVarsE

runEnvVarsE ::
  IOE :> es =>
  Eff (EnvVarsE : es) a ->
  Eff es a
runEnvVarsE = reinterpret loadEnvVars $ \_ -> \case
  AskEnvVar f -> asks f

loadEnvVars ::
  IOE :> es =>
  Eff (Reader EnvVars : es) a ->
  Eff es a
loadEnvVars m = do
  envVars <- liftIO $ Env.parse (header "nixos-pods-tool") $ do
    serviceImagesDir <-
      var
        (str <=< nonempty)
        "SERVICE_IMAGES_DIR"
        (help "The directory containing docker images for our services")
    skopeoPath <- var (str <=< nonempty) "SKOPEO_PATH" (help "Path of skopeo executable")
    pure EnvVars {serviceImagesDir, skopeoPath}
  runReader envVars m
