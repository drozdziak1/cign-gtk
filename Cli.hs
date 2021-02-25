module Cli where

import Options.Applicative

data CignGtkOpts = CignGtkOpts
  { extraCignArgs :: String
  }
  deriving (Show)

cliParser :: Parser CignGtkOpts
cliParser =
  CignGtkOpts
    <$> strOption
      ( long "args"
          <> metavar "EXTRA_CIGN_ARGS"
          <> value ""
          <> help "Extra args to pass to cign"
      )

opts :: ParserInfo CignGtkOpts
opts =
  info
    (cliParser <**> helper)
    ( fullDesc
        <> progDesc "Can I Go Now? - The GTK frontend"
        <> header "Puts your git worries to sleep"
    )
