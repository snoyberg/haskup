{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
import RIO
import RIO.Process (exec)
import RIO.FilePath (stripExtension)
import System.Environment (getArgs, getProgName, getExecutablePath)
import Haskup

main :: IO ()
main = runHaskup $ do
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
  withToolchain (toolchainGhc ghcVer) $ do
    eexe <- findHaskupExe $ fromMaybe exeName $ stripExtension ".exe" exeName
    exe <- either throwIO pure eexe
    self <- liftIO getExecutablePath
    when (exe == self) $ error "haskwrap is calling itself, exiting"
    exec exe args
