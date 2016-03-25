{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-} 
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module FsMain where

import System.Fuse (fuseMain, defaultExceptionHandler)

import Adb (ifAdbPresent)
import Fs 

import qualified System.Environment as Env
import Control.Monad (join, void)

import System.Console.GetOpt
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)

import Data.Maybe (isJust)
import Data.List (find)

import Prelude hiding (log)

main :: IO ()
main = do
  args <- Env.getArgs

  currentDir <- getCurrentDirectory

  case getOpt Permute options args of
    (opts, left, []) -> do

         if Help `elem` opts 
         then printHelp 
         else case createLogFunc currentDir opts of
                Right logFunc -> 
                    Env.withArgs left $ void $ ifAdbPresent $ fuseMain (adbFSOps logFunc) defaultExceptionHandler

                Left error -> ioError $ userError error

    (_, _, errors) -> ioError (userError $ concat errors)

help :: String
help = usageInfo "Usage: fuse_adb_fs [OPTION...] mountpoint" options

printHelp :: IO ()
printHelp = putStrLn help

data LogLevel = LogSilent
              | LogFailsOnly
              | LogFull
                deriving (Show, Eq)

data Option = LogLevel (Maybe LogLevel)
            | LoggingFile String
            | Help
              deriving (Show, Eq)


options :: [OptDescr Option]
options = [ Option ['l'] ["loglevel"] (ReqArg toLogLevel "MODE")  "silent, fails, full"
          , Option ['f'] ["logfile"]  (ReqArg LoggingFile "FILE") "write log to FILE" 
          , Option ['h'] ["help"]     (NoArg Help)                "show help" ]
    where toLogLevel "silent" = ok LogSilent
          toLogLevel "fails"  = ok LogFailsOnly
          toLogLevel "full"   = ok LogFull
          toLogLevel _        = LogLevel Nothing
          ok = LogLevel . Just

createLogFunc :: FilePath -> [Option] -> Either String LogFunction
createLogFunc currentDir options 
    | Nothing <- logLevel
    , Nothing <- logFile 
    = noLogging

    | Just _ <- logLevel
    , Nothing <- logFile
    = Left "if log level specified, log file should be specified too"

    | Nothing <- logLevel
    , Just file <- logFile
    = logFull file

    | Just level <- logLevel
    , Just file <- logFile
    = case level of 
        Just LogSilent -> noLogging
        Just LogFailsOnly -> logFails file
        Just LogFull -> logFull file
        Nothing -> logFull file

    where toLogLevel (LogLevel l) = Just l
          toLogLevel _ = Nothing

          toLogFile (LoggingFile f) = Just f
          toLogFile _ = Nothing

          logLevel = fnd toLogLevel
          logFile = fnd toLogFile

          noLogging = Right $ const $ const $ return ()

          logFull = doLog $ either Just Just
          logFails = doLog $ either Just $ const Nothing

          doLog :: (Either String String -> Maybe String) -> String -> Either String LogFunction
          doLog filter file = 
              Right $ \resultMsg -> \logged -> 
                                    case filter resultMsg of
                                         Just msg -> appendFile targetPath $ concat logged ++ "\n" ++ msg ++ "\n------------------"
                                         _ -> return ()
                  where targetPath = currentDir </> file

          fnd f = join $ find isJust $ map f options

