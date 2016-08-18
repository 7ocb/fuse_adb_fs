{-# LANGUAGE TypeSynonymInstances #-} 
module Types
    (RemoteFsEntry(..)
    , PathQualification(..)
    , isSymlink)
where
    
import System.Posix.Types
import System.Posix.Files

data RemoteFsEntry = RemoteFsEntry { rfseMode :: FileMode
                                   , rfseSize :: Integer
                                   , rfseName :: String }
                   deriving (Show, Eq)

instance Monoid FileMode where
    mempty = nullFileMode
    mappend = unionFileModes

isSymlink :: FileMode -> Bool
isSymlink = (/= nullFileMode) . intersectFileModes symbolicLinkMode

data PathQualification = FsRoot
                       | Device String
                       | DeviceFs String
                       | InDeviceFs String String
                         deriving (Show, Eq)