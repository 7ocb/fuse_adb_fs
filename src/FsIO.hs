{-|
Module FsIO

This module provides monad to isolate code of adb fs from actually doing IO
-}
module FsIO (FsIO(..)) where 

import qualified Data.ByteString.Char8 as B

class (Monad m) => ShellCall m where
    -- | perform shell call
    shellCall :: [String] -- ^ args
                 -> m (Either String String) -- ^ error or success    


class (ShellCall m) => FsIO m where
    -- | read file in local fs
    readLocalFile :: FilePath -> m (Either String B.ByteString)

    -- | write file in local fs, returns error if any, nothing on success
    writeLocalFile :: FilePath -> m (Maybe String)

    -- | call with temp file path
    withTempFile :: FilePath -- ^ the base path, will be prepended to the file
                 -> (FilePath -> m ()) -- ^ action to cleanup file
                 -> (FilePath -> m a) -- ^ action to be called with temp file path
                 -> m a 

    deleteLocalFile :: FilePath -> m ()
