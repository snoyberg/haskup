{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

import RIO
import Haskup (runHaskup)
import Development.NSIS
import Development.NSIS.Plugins.EnvVarUpdate
import Network.HTTP.Download (verifiedDownload, mkDownloadRequest, setLengthCheck, CheckHexDigest (CheckHexDigestString), HashCheck (HashCheck), setHashChecks)
import Path (Path, Abs, Rel, File, Dir, mkRelFile, mkRelDir, (</>))
import Path.IO (makeAbsolute)
import qualified Crypto.Hash.Algorithms

-- Note that it is *required* to use a NSIS compiler that supports long strings,
-- to avoid corrupting the user's $PATH.

executables :: [FilePath]
executables =
  [ "haskup.exe"
  , "cabal.exe"
  , "stack.exe"
  ]

data Download = Download
  { url :: !String
  , size :: !Int
  , sha256 :: !String
  , filename :: !(Path Rel File)
  }

downloads :: [Download]
downloads =
  [ Download
      { url = "https://github.com/commercialhaskell/stack/releases/download/v2.7.1/stack-2.7.1-windows-x86_64-bin.exe"
      , size = 59980288
      , sha256 = "1a15494065de2f72fe1e35d208f2c3c874801ef92df3c8804e910e1de0a93f05"
      , filename = $(mkRelFile "stack.exe")
      }
  , Download
      { url = "https://s3.amazonaws.com/download.fpcomplete.com/cabal/cabal-install-3.4.0.0-x86_64-windows.exe"
      , size = 87932677
      , sha256 = "5effd1993aff5e5825bd408a1ec1aa382ffd59b511bc1680a0527f3dbcc173e7"
      , filename = $(mkRelFile "cabal.exe")
      }
  ]

main :: IO ()
main = runHaskup $ do
  destDir :: Path Abs Dir <- makeAbsolute ($(mkRelDir "bin") :: Path Rel Dir)
  for_ downloads $ \download -> do
    let dreq = setHashChecks [HashCheck Crypto.Hash.Algorithms.SHA256 (CheckHexDigestString (sha256 download))]
             $ setLengthCheck (Just (size download))
             $ mkDownloadRequest
             $ fromString
             $ url download
    let dest = destDir </> (filename download)
    void $ verifiedDownload dreq dest (\_ -> pure ())
  writeFileUtf8 "bin/haskup-installer.nsi" $ fromString $ nsis $ do
    _ <- constantStr "Name" "haskup"

    name "$Name"
    outFile "haskup-installer.exe"
    installDir "$APPDATA/local/bin"
    installDirRegKey HKCU "Software/$Name" "Install_Dir"
    requestExecutionLevel User

    page Directory
    page Components
    page InstFiles

    unpage Components
    unpage InstFiles

    void $ section "Install haskup executables" [Required] $ do
      setOutPath "$INSTDIR"
      for_ executables $ \exe ->
        file [OName $ fromString exe] $ fromString exe

      -- Write the installation path into the registry
      writeRegStr HKCU "SOFTWARE/$Name" "Install_Dir" "$INSTDIR"

      -- Write the uninstall keys for Windows
      writeRegStr HKCU "Software/Microsoft/Windows/CurrentVersion/Uninstall/$Name" "DisplayName" "$Name"
      writeRegStr HKCU "Software/Microsoft/Windows/CurrentVersion/Uninstall/$Name" "UninstallString" "\"$INSTDIR/uninstall-haskup.exe\""
      writeRegDWORD HKCU "Software/Microsoft/Windows/CurrentVersion/Uninstall/$Name" "NoModify" 1
      writeRegDWORD HKCU "Software/Microsoft/Windows/CurrentVersion/Uninstall/$Name" "NoRepair" 1
      writeUninstaller "uninstall-haskup.exe"

    void $ section "Add to user %PATH%"
      [ Description "Add installation directory to user %PATH% to allow running haskup in the console."
      ] $ do
        setEnvVarPrepend HKCU "PATH" "$INSTDIR"

    -- Uninstallation sections. (Any section prepended with "un." is an
    -- uninstallation option.)
    section "un.haskup" [] $ do
      deleteRegKey HKCU "Software/Microsoft/Windows/CurrentVersion/Uninstall/$Name"
      deleteRegKey HKCU "Software/$Name"

      for_ executables $ \exe ->
        delete [] $ fromString $ "$INSTDIR/" ++ exe
      delete [] "$INSTDIR/uninstall-haskup.exe"
      rmdir [] "$INSTDIR" -- will not remove if not empty

    {- These changes will break Stack installs on uninstall, so leaving them out

    -- The description text is not actually added to the uninstaller as of
    -- nsis-0.3
    section "un.Install location on %PATH%"
      [ Description "Remove $INSTDIR from the user %PATH%. There may be other programs installed in that location."
      ] $ do
        setEnvVarRemove HKCU "PATH" "$INSTDIR"
        
    section "un.Compilers installed by stack"
      [ Unselected
      , Description "Remove %LOCALAPPDATA%/Programs/stack, which contains compilers that have been installed by Stack."
      ] $ do
        rmdir [Recursive] "$LOCALAPPDATA/Programs/stack"

    section "un.stack snapshots and configuration"
      [ Unselected
      , Description "Remove %APPDATA%/stack, which contains the user-defined global stack.yaml and the snapshot/compilation cache."
      ] $ do
        rmdir [Recursive] "$APPDATA/stack"

    -}
