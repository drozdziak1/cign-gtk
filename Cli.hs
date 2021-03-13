module Cli where

import Options.Applicative

data CignGtkOpts = CignGtkOpts
  { extraCignArgs :: String,
    successCommand :: String
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
    <*> strOption
      ( long "exec"
          <> short 'x'
          <> metavar "COMMAND"
          <> value ""
          <> help "A command to execute when cign output is clean or ignored"
      )

opts :: ParserInfo CignGtkOpts
opts =
  info
    (cliParser <**> helper)
    ( fullDesc
        <> progDesc "Can I Go Now? - The GTK frontend"
        <> header "Puts your git worries to sleep"
    )
