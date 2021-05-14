{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Haskup
  ( -- * Haskup monad
    Haskup
  , runHaskup
  , binDirsL
    -- * Install toolchain
  , Toolchain
  , toolchainGhc
  , withToolchain
    -- * Use toolchain
  , findHaskupExe
  , execHaskupExe
    -- * Configuration
  , defaultGhcVer
    -- * Helper reexports
  , Version
  , parseVersionThrowing
    -- * Error handling
  , HaskupException (..)
  ) where

import RIO
import RIO.Process (exec, augmentPathMap, withModifyEnvVars, HasProcessContext (..))
import RIO.PrettyPrint (HasTerm (..), HasStylesUpdate (..))
import RIO.Directory (findExecutable, findExecutablesInDirectories, canonicalizePath)
import RIO.FilePath (isAbsolute)
import Stack.Setup
import Stack.Runners
import Stack.Types.Config
import Stack.Types.Version (Version, VersionCheck (..))
import Stack.Options.GlobalParser
import Stack.Prelude (WantedCompiler (..), parseVersionThrowing, toFilePath)
import System.Terminal (hIsTerminalDeviceOrMinTTY)
import Pantry (HasPantryConfig (..))

-- | Environment for this library, should be initialized with 'runHaskup'.
--
-- @since 0.1.0.0
data Haskup = Haskup
  { _bc :: !BuildConfig
  , _haskupBinDirs :: ![FilePath]
  }

instance HasLogFunc Haskup where
  logFuncL = buildConfigL.logFuncL
instance HasProcessContext Haskup where
  processContextL = buildConfigL.processContextL
instance HasRunner Haskup where
  runnerL = buildConfigL.runnerL
instance HasPlatform Haskup where
instance HasGHCVariant Haskup where
instance HasTerm Haskup where
  useColorL = buildConfigL.useColorL
  termWidthL = buildConfigL.termWidthL
instance HasPantryConfig Haskup where
  pantryConfigL = buildConfigL.pantryConfigL
instance HasStylesUpdate Haskup where
  stylesUpdateL = buildConfigL.stylesUpdateL
instance HasConfig Haskup where
  configL = buildConfigL.configL
instance HasBuildConfig Haskup where
  buildConfigL = lens _bc (\x y -> x { _bc = y })

-- | Lens for all additional directories added to the @PATH@.
--
-- This can be useful for ensuring you're looking for newly provided executables.
--
-- @since 0.1.0.0
binDirsL :: Lens' Haskup [FilePath]
binDirsL = lens _haskupBinDirs (\x y -> x { _haskupBinDirs = y })

getDefaultTerminal :: IO Bool
getDefaultTerminal = hIsTerminalDeviceOrMinTTY stdout

-- | Initialize the @Haskup@ environment with default settings.
--
-- Note that this does not provide any toolchains! You'll need to use 'withToolchain' to make that happen.
--
-- @since 0.1.0.0
runHaskup :: MonadIO m => RIO Haskup a -> m a
runHaskup inner = liftIO $ do
  defaultTerminal <- getDefaultTerminal
  gopts <- globalOptsFromMonoid defaultTerminal mempty
  withRunnerGlobal gopts $ withConfig NoReexec $ withBuildConfig $ do
      bc <- ask
      let haskup = Haskup
            { _bc = bc
            , _haskupBinDirs = []
            }
      runRIO haskup inner

-- | Run the given subcommand with the specified toolchain available.
--
-- Said another way: "install GHC and then run this action."
--
-- @since 0.1.0.0
withToolchain :: Toolchain -> RIO Haskup a -> RIO Haskup a
withToolchain (ToolchainGhc ghcVer) inner = do
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
  local (over binDirsL (binDirs ++)) $
    withModifyEnvVars (either impureThrow id . augmentPathMap binDirs) inner

-- | Specifies which toolchain to install.
--
-- Constructors are not exposed to make it possible to extend in a backwards compatible way in the future.
--
-- See smart constructors like 'toolchainGhc'.
--
-- @since 0.1.0.0
data Toolchain = ToolchainGhc !Version

-- | A toolchain providing the given GHC version.
--
-- @since 0.1.0.0
toolchainGhc :: Version -> Toolchain
toolchainGhc = ToolchainGhc

-- | Find the given executable as provided by haskup, if possible.
--
-- This function will prefer executables present in the 'binDirsL'.
--
-- On Windows, ensure that you do not pass in @.exe@ or other file extensions.
--
-- @since 0.1.0.0
findHaskupExe :: String -> RIO Haskup (Either HaskupException FilePath)
findHaskupExe name
  | isAbsolute name = (Right <$> canonicalizePath name) `catchIO` \e -> pure $ Left $ AbsolutePathDoesNotExist name e
  | otherwise = do
      extraBin <- view binDirsL
      exes <- findExecutablesInDirectories extraBin name
      case exes of
        -- Generating a runtime exception here, since canonicalize is
        -- expected to succeed in this case (we already found an executable)
        exe:_ -> Right <$> canonicalizePath exe
        [] -> do
          mexe <- findExecutable name
          case mexe of
            Nothing -> pure $ Left $ ExeNotFound name
            Just exe -> Right <$> canonicalizePath exe

-- | Exception for things that can go wrong in this library.
--
-- Note that there is /no guarantee/ that other exceptions will not occur!
--
-- @since 0.1.0.0
data HaskupException
  = ExeNotFound !String
  | AbsolutePathDoesNotExist !FilePath !IOException
  deriving (Show, Typeable)
instance Exception HaskupException

-- | Attempt to execute the given executable with the provided arguments.
--
-- Leverages 'findHaskupExe' and 'exec'.
--
-- @since 0.1.0.0
execHaskupExe :: String -> [String] -> RIO Haskup (Either HaskupException void)
execHaskupExe name args = findHaskupExe name >>= traverse (\exe -> exec exe args)

-- | Determine the default GHC version to be used for haskup when no override is present.
--
-- @since 0.1.0.0
defaultGhcVer :: RIO Haskup Version
defaultGhcVer = do
  logWarn "This is really dumb, just defaulting to GHC 8.10.4 for now"
  parseVersionThrowing "8.10.4"
