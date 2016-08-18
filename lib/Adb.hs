{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Adb ( isAdbPresent
           , ifAdbPresent
           , callAdb
           , restartAdb
           , callAdbIO
           , restartAdbIO
           , callForDevice
           , queryDevice
           , Information(..)
           , Device
           , deviceType
           , serialNo
           , model
           , device
           , DeviceType(..)
           , listDevices
           , startAdbProcess
           , MonadAdb
           , AdbFail )
where

import Data.List.Split (wordsBy, splitOn)
import Data.List (isPrefixOf, find)
import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)
import Control.Monad.Except
import Control.Exception (catch, IOException)  
import System.Process as Proc
import System.Exit

import Control.Monad.Writer

data Information = Logcat
                 | Bugreport


class Monad m => MonadAdb m where
    callAdb   :: [String] -> m String
    restartAdb :: m ()

class Monad m => AdbFail m where
    adbFail :: String -> m a

callAdbIO :: (MonadIO m) => [String] -> m (Either String String)
callAdbIO parameters = do 
  liftIO $ ( Right <$> (readProcess "adb" parameters "") ) 
             `catch`
             ((return . Left . show) :: IOException -> IO (Either String String))


restartAdbIO :: (MonadError String m, MonadIO m) => m ()
restartAdbIO = ((&&) <$> stop <*> start) >>= result
    where result True = return ()
          result False = throwError "can't restart adb"

          stop = command "kill-server"
          start = command "start-server"
          command = (liftM (== ExitSuccess)) . liftIO . Proc.system . ("sudo adb " ++)


instance MonadAdb (ExceptT String IO) where 
    callAdb params = do 
      value <- callAdbIO params

      either throwError return value

    restartAdb = restartAdbIO

instance AdbFail (ExceptT String IO) where 
    adbFail v = throwError v

callForDevice :: (MonadAdb m) => Device -> [String] -> m String
callForDevice device command = 
    callAdb $ ["-s", serialNo device] ++ command

ifAdbPresent :: IO () -> IO ()
ifAdbPresent action = do
  canWork <- runExceptT isAdbPresent
  case canWork of
    Left e -> putStrLn $ "can't execute adb: " ++ e
    Right False -> putStrLn $ "adb version is empty, does adb work?"
    Right True -> action

isAdbPresent :: (MonadAdb m) => m Bool
isAdbPresent = liftM (/= "") $ callAdb ["version"]
            
queryDevice :: (MonadError String m, MonadAdb m) => Device -> Information -> m String
queryDevice device info =
    callAdb $ ["-s", serialNo device] ++ (args info)

    where args Logcat    = ["logcat", "-d"]
          args Bugreport = ["bugreport"]

startAdbProcess :: Device -> String -> IO Proc.ProcessHandle
startAdbProcess device command = Proc.runCommand $ "adb -s " ++ (serialNo device) ++ " " ++ command 

isUndefined :: Device -> Bool             
isUndefined = isPrefixOf "???" . serialNo

deviceFromDeviceString :: String -> Device
deviceFromDeviceString string =

    Device serial
               (additional "model")
               (additional "device")
               deviceType
    
    where serial:_:additionalInfo = wordsBy (`elem` "\t ") string

          deviceType | "emulator-" `isPrefixOf` serial = Emulator
                     | otherwise                       = RealDevice

          infoPairs = map (splitOn ":") additionalInfo
              
          additional key = last `fmap` find ((key ==) . head) infoPairs

                                                         

              
parseAdbDevicesOutput :: String -> [Device]              
parseAdbDevicesOutput output =
    map deviceFromDeviceString $ filter (not . invalidDeviceLine) $ lines output
    where invalidDeviceLine ('*':rest) = True
          invalidDeviceLine serial = null serial
                                   || "List of devices" `isPrefixOf` serial
                           
listDevices :: (AdbFail m, MonadAdb m) => m [Device]
listDevices = list $ do restartAdb
                        list $ adbFail "There is undefined devices, but restarting not helped"

    where list onFail = do 
            adbDevices <- liftM parseAdbDevicesOutput $ callAdb ["devices", "-l"]
            
            if any isUndefined adbDevices
            then onFail
            else return adbDevices
            

data DeviceType = Emulator
                | RealDevice
                  deriving (Show, Eq)

data Device = Device {
      serialNo   :: String,
      model      :: Maybe String,
      device     :: Maybe String,
      deviceType :: DeviceType
    } deriving (Show, Eq)