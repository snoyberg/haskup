{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import RIO
import Data.String
import System.Environment

import Development.NSIS
import Development.NSIS.Plugins.EnvVarUpdate

-- Note that it is *required* to use a NSIS compiler that supports long strings,
-- to avoid corrupting the user's $PATH.

executables :: [FilePath]
executables =
  [ "haskup.exe"
  ]

main :: IO ()
main = do
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

    section "Install haskup executables" [Required] $ do
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

    section "Add to user %PATH%"
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
