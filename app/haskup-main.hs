{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import RIO
import Haskup
import Options.Applicative.Simple
import qualified Paths_haskup
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Writer (Writer)

data Global = Global
  { ghcVersion :: !(Maybe String)
  }

data Command
  = CommandInstall
  | CommandRun !RunOpts

data RunOpts = RunOpts
  { roCommand :: !String
  , roArgs :: ![String]
  }

globalParser :: Parser Global
globalParser = Global
  <$> optional (strOption (long "ghc" <> metavar "GHC-VERSION"))

commandParser :: ExceptT Command (Writer (Mod CommandFields Command)) ()
commandParser = do
  addCommand "install" "Install a Haskell toolchain" id $ pure CommandInstall
  addCommand "exec" "Execute an arbitrary command" CommandRun $ RunOpts
    <$> strArgument (metavar "COMMAND")
    <*> args
  addHelper "ghc" "Run the ghc executable"
  addHelper "ghci" "Run the ghci executable"
  addHelper "runghc" "Run the runghc executable"
  addHelper "cabal" "Run the cabal executable"
  addHelper "stack" "Run the stack executable"
  where
    addHelper cmd desc = addCommand cmd desc CommandRun $ RunOpts cmd
      <$> args
    args = many (strArgument (metavar "ARGUMENT"))

main :: IO ()
main = runHaskup $ do
  (global, cmd) <- liftIO $ simpleOptions
    $(simpleVersion Paths_haskup.version)
    "haskup Haskell toolchain installer"
    "Installs, manages, and runs your Haskell toolchains"
    globalParser
    commandParser
  ghcVer <- maybe defaultGhcVer parseVersionThrowing $ ghcVersion global
  withToolchain (toolchainGhc ghcVer) $
    case cmd of
      CommandInstall -> logInfo "Installation complete!"
      CommandRun ro -> execHaskupExe (roCommand ro) (roArgs ro) >>= either throwIO pure
