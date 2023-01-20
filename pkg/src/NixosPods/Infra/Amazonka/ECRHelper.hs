{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NixosPods.Infra.Amazonka.ECRHelper
  ( ECRHelper (..),
    uploadImages,
    runECRHelper,
  )
where

import Amazonka.ECR (newGetAuthorizationToken)
import Control.Exception.Safe
import Control.Lens
import Data.Generics.Labels ()
import Data.String.Interpolate (i, iii)
import Data.Text qualified as T
import Data.Text.Encoding.Base64 qualified as Base64
import NixosPods.Infra.Amazonka (Amazonka)
import NixosPods.Infra.Amazonka qualified as Amazonka
import NixosPods.Infra.Clock (Clock, getCurrentTime, unixTimestampNano)
import NixosPods.Prelude
import UnliftIO.Process (callCommand)

data ECRHelper :: Effect where
  UploadImages ::
    -- [(image path, ECR repository uri)]
    [(FilePath, Text)] ->
    -- returns uploaded image uris
    ECRHelper m [Text]

makeEffect ''ECRHelper

data Error
  = AuthTokenDecodeError {reason :: Text}
  | AuthTokenNotFoundError
  deriving stock (Show)

instance Exception Error where
  displayException (AuthTokenDecodeError {..}) = [i|Failed to decode ECR auth token, reason: #{reason}|]
  displayException AuthTokenNotFoundError = [i|Can't load ECR auth token|]

runECRHelper ::
  ( IOE :> es,
    Clock :> es,
    Amazonka :> es
  ) =>
  Eff (ECRHelper : es) a ->
  Eff es a
runECRHelper = interpret $ \_ -> \case
  UploadImages pathToUri -> do
    timestamp <- unixTimestampNano <$> getCurrentTime
    ECRCredentials {username, password} <- getECRCredentials
    forM pathToUri $ \(imagePath, uri) -> do
      callCommand
        [iii|skopeo --insecure-policy copy
          --dest-username '#{username}'
          --dest-password '#{password}'
          docker-archive:#{imagePath}
          docker://#{uri}:#{timestamp}
        |]
      pure [i|#{uri}:#{timestamp}|]

data ECRCredentials = ECRCredentials
  { username :: Text,
    password :: Text
  }
  deriving stock (Show, Eq, Generic)

getECRCredentials ::
  ( Amazonka :> es
  ) =>
  Eff es ECRCredentials
getECRCredentials = do
  resp <- Amazonka.send newGetAuthorizationToken
  tokenBase64 <-
    maybe (throw AuthTokenNotFoundError) pure $
      resp ^? #authorizationData . _Just . _head . #authorizationToken . _Just
  nameAndPassword <-
    either (\msg -> throw AuthTokenDecodeError {reason = msg}) pure $
      Base64.decodeBase64 tokenBase64
  case T.splitOn ":" nameAndPassword of
    [name, pwd] -> pure $ ECRCredentials {username = name, password = pwd}
    _ -> throw AuthTokenDecodeError {reason = "when decoded, it must be in the form name:pwd"}
