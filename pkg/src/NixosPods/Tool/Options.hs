{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module NixosPods.Tool.Options
  ( Options (..),
    Command (..),
    OptionsE (..),
    getOptions,
    runOptionsE,
  )
where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Dynamic
import Effectful.TH
import Options.Applicative
import Relude hiding (Reader, ask, runReader)

data Options = Options
  { command :: Command,
    stackNamePrefix :: Text
  }
  deriving stock (Show, Eq, Generic)

data Command = CommandDeploy
  deriving stock (Show, Eq, Generic)

readOptions :: IO Options
readOptions = execParser opts
  where
    opts =
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Tool for working with NixOS Pods"
        )

optionsParser :: Parser Options
optionsParser =
  Options
    <$> subparser
      ( command "deploy" (info commandDeployParser (progDesc "Deploy NixOS Pods to AWS"))
      )
    <*> strOption
      ( long "stack-name-prefix"
          <> metavar "TEXT"
          <> help "Prefix for all the CloudFormation stacks"
          <> value "nixos"
      )

commandDeployParser :: Parser Command
commandDeployParser = pure CommandDeploy

data OptionsE :: Effect where
  GetOptions :: OptionsE m Options

makeEffect ''OptionsE

runOptionsE ::
  IOE :> es =>
  Eff (OptionsE : es) a ->
  Eff es a
runOptionsE = reinterpret initOptionsE $ \_ -> \case
  GetOptions -> ask

initOptionsE ::
  IOE :> es =>
  Eff (Reader Options : es) a ->
  Eff es a
initOptionsE eff = do
  opts <- liftIO readOptions
  runReader opts eff