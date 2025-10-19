module Cli (runCli, Cmd (..), Opts (..), Cli (..)) where

import Options.Applicative

data RunFileOpts = RunFileOpts
  { filePath :: FilePath
  }
  deriving (Eq, Show)

data Cmd
  = Repl
  | RunFile RunFileOpts
  deriving (Eq, Show)

data Opts = Opts
  { verbose :: Bool
  }
  deriving (Eq, Show)

data Cli = Cli
  { cmd :: Cmd,
    opts :: Opts
  }
  deriving (Eq, Show)

replDesc, runFileDesc :: String
replDesc = "Start an interactive REPL session"
runFileDesc = "Run commands from a specified file"

replParser, runFileParser, cmdParser :: Parser Cmd
replParser = pure Repl
runFileParser = RunFile <$> RunFileOpts <$> strArgument (metavar "FILE" <> help "Path to the input file")
cmdParser =
  hsubparser
    ( command "repl" (info replParser (progDesc replDesc))
        <> command "run" (info runFileParser (progDesc runFileDesc))
    )

optsParser :: Parser Opts
optsParser =
  Opts
    <$> switch (long "verbose" <> short 'v' <> help "Enable verbose output")

cliParser :: Parser Cli
cliParser =
  Cli
    <$> cmdParser
    <*> optsParser

cliDesc :: String
cliDesc = "Symbolic computation with e-graphs"

runCli :: IO Cli
runCli = customExecParser p pinfo
  where
    pinfo = info (cliParser <**> helper) (progDesc cliDesc)
    p = prefs showHelpOnEmpty
