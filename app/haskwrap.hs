{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
import RIO
import RIO.Process
import Stack.Setup
import Stack.Runners
import Stack.Types.Config
import Stack.Types.Version (Version, VersionCheck (..))
import Stack.Options.GlobalParser
import Stack.Prelude (WantedCompiler (..), parseVersionThrowing, toFilePath)
import System.Environment (getArgs, getProgName)
import System.Terminal (hIsTerminalDeviceOrMinTTY)

getDefaultTerminal :: IO Bool
getDefaultTerminal = hIsTerminalDeviceOrMinTTY stdout

runBuildConfig :: RIO BuildConfig a -> IO a
runBuildConfig inner = do
  defaultTerminal <- getDefaultTerminal
  gopts <- globalOptsFromMonoid defaultTerminal mempty
  withRunnerGlobal gopts $ withConfig NoReexec $ withBuildConfig inner

main :: IO ()
main = runBuildConfig $ do
  args0 <- liftIO getArgs
  (args1, ghcVer) <-
    case args0 of
      "--ghc":args1 ->
        case args1 of
          [] -> error "--ghc must be followed by a GHC version number"
          verS:args -> do
            ghcVer <- parseVersionThrowing verS
            pure (args, ghcVer)
      _ -> do
        ghcVer <- defaultGhcVer
        pure (args0, ghcVer)
  (exeName, args) <-
    case args1 of
      "exec":args2 ->
        case args2 of
          [] -> error "exec must be followed by a command"
          exeName:args -> pure (exeName, args)
      _ -> do
        exeName <- liftIO getProgName
        pure (exeName, args1)
  logInfo $ "exeName is " <> fromString exeName
  let sopts = SetupOpts
        { soptsInstallIfMissing = True
        , soptsUseSystem = False
        , soptsWantedCompiler = WCGhc ghcVer
        , soptsCompilerCheck = MatchExact
        , soptsStackYaml = Nothing
        , soptsForceReinstall = False
        , soptsSanityCheck = False
        , soptsSkipGhcCheck = False
        , soptsSkipMsys = False
        , soptsResolveMissingGHC = Nothing
        , soptsGHCBindistURL = Nothing
        }
  (_, extraDirs) <- ensureCompilerAndMsys sopts
  let binDirs = map toFilePath $ edBins extraDirs
  withModifyEnvVars (either impureThrow id . augmentPathMap binDirs) $ exec exeName args

defaultGhcVer :: RIO BuildConfig Version
defaultGhcVer = do
  logWarn "This is really dumb, just defaulting to GHC 8.10.4 for now"
  parseVersionThrowing "8.10.4"
