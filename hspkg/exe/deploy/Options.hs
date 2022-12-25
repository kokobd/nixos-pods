{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Options
  ( Options (..),
    Command (..),
    parseOptions,
  )
where

import Options.Applicative
  ( Parser,
    command,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    progDesc,
    strOption,
    subparser,
    value,
  )
import Relude

data Options = Options
  { command :: Command,
    stackNamePrefix :: Text,
    dhallFile :: FilePath
  }
  deriving stock (Show, Eq, Generic)

data Command
  = Base
  | Controller
  deriving stock (Show, Eq, Generic)

parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts =
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "A tool for deploying nixos-pods"
        )

optionsParser :: Parser Options
optionsParser =
  Options
    <$> subparser
      ( command "base" (info baseCommandParser (progDesc "hello"))
      )
    <*> strOption (long "stack-name-prefix" <> value "devbox" <> help "prefix of cloudformation stack names")
    <*> strOption (long "dhall-file" <> help "location of base.dhall")

baseCommandParser :: Parser Command
baseCommandParser = pure Base
