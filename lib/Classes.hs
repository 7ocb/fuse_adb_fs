module Classes 
    (GetFuseContext,
     fuseUID,
     fuseGID,
     WithCurrentDevice,
     currentDevice,
     callCurrentDevice,
     Logging,
     writeLog,
     CanFail,
     failWith,
     Error(..))
where

import System.Fuse (FuseContext)
import Adb (Device)
import Foreign.C.Error (Errno(..))
import System.Posix.Types (UserID, GroupID)

newtype Error = Error { eErrno ::  Errno }
    deriving (Show, Eq)

instance Show Errno where
    show (Errno c) = show c

class (Monad m) => GetFuseContext m where
    fuseUID :: m UserID
    fuseGID :: m GroupID

class (Monad m) => WithCurrentDevice m where
    currentDevice :: m Device
    callCurrentDevice :: [String] -> m String

class (Monad m) => Logging m where
    writeLog :: String -> m ()

class (Monad m) => CanFail m where
    failWith :: Error -> m a
