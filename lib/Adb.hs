{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Adb (isAdbPresent,
            ifAdbPresent,
            runAdbIO,
            callAdb,
            callForDevice,
            queryDevice,
            Information(..),
            Device,
            deviceType,
            serialNo,
            model,
            device,
            DeviceType(..),
            listDevices,
            startAdbProcess,
            MonadAdb)
where

import Data.List.Split (wordsBy, splitOn)
import Data.List (isPrefixOf, find)
import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)
import Control.Exception (catch, IOException)  
import System.Process as Proc
import System.Exit

import Control.Monad.Writer

data Information = Logcat
                 | Bugreport

class Monad m => MonadAdb m where
    callAdb :: [String] -> m String
    restartAdb :: m Bool


newtype AdbIO a = A { runAdbIO :: IO a } 
    deriving (Monad)

instance Functor AdbIO where
    fmap = liftM
 
instance Applicative AdbIO where
    pure  = return
    (<*>) = ap

instance MonadAdb AdbIO where
    callAdb parameters = A (catch (readProcess "adb" parameters "")
                                      ((\e -> return "") :: IOException -> IO String))
    restartAdb = A $ do
                   let isSuccess = liftM (== ExitSuccess)

                   stoppingSucceed <- isSuccess $ Proc.system "sudo adb kill-server"
                   if stoppingSucceed == True
                   then isSuccess $ Proc.system "sudo adb start-server"
                   else return False

callForDevice :: MonadAdb m => Device -> [String] -> m String
callForDevice device command = 
    callAdb $ ["-s", serialNo device] ++ command

ifAdbPresent :: IO () -> IO ()
ifAdbPresent action = do
  canWork <- runAdbIO $ isAdbPresent
  if canWork
  then action
  else putStrLn "adb command not found in path"
                   
isAdbPresent :: MonadAdb m => m Bool
isAdbPresent = liftM (/= "") $ callAdb ["get-state"]
            
queryDevice :: MonadAdb m => Device -> Information -> m String
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
                           
listDevices :: MonadAdb m => m (Either String [Device])
listDevices =
    listDevices' True
    where listDevices' restartAdbOnFail = do
              adbDevices <- liftM parseAdbDevicesOutput $ callAdb ["devices", "-l"]
              if any isUndefined adbDevices
              then if restartAdbOnFail
                   then do restartResult <- restartAdb
                           if restartResult == True
                           then listDevices' False
                           else return $ Left "Needed to restart adb, but failed"
                           
                   else return $ Left "There is undefined devices, but restarting not helped"
                  
              else return $ Right adbDevices

data DeviceType = Emulator
                | RealDevice
                  deriving (Show, Eq)

data Device = Device {
      serialNo   :: String,
      model      :: Maybe String,
      device     :: Maybe String,
      deviceType :: DeviceType
    } deriving (Show, Eq)