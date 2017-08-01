{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-} 
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Fs (adbFSOps, 
           LogFunction, 
           PathQualification(..),
           qualifyPath,
           emptyDirContents) where

import System.Fuse

import Classes

import qualified Data.ByteString.Char8 as B
import Adb (serialNo, ifAdbPresent, Device, MonadAdb, AdbFail)
import qualified Adb as Adb

import qualified System.Environment as Env
import System.Directory (removeFile)
import System.Random (randomRIO)
import Control.Monad (forM, void)
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Applicative ((<*))

import qualified Utils as U

import Types

import Parsers (Parser)
import qualified Parsers as P

import System.FilePath

import System.Console.GetOpt

import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO

import Data.Maybe (fromMaybe, isJust)
import Data.List (find, intercalate)

import Prelude hiding (log)

type FileHandle = (DeviceId, FilePath)

type DeviceId = String
type FsCall a = IO (Either Error a)
type FilesList = [(FilePath, FileStat)]

type AdbFsCall = ExceptT Error (WriterT [String] IO)



type DeviceCall = ReaderT Device AdbFsCall

getFuseUID :: (MonadIO m) => m UserID
getFuseUID = fuseCtxUserID <$> (liftIO $ getFuseContext)

getFuseGID :: (MonadIO m) => m GroupID
getFuseGID = fuseCtxGroupID <$> (liftIO $ getFuseContext)

instance GetFuseContext AdbFsCall where 
    fuseUID = getFuseUID
    fuseGID = getFuseGID

instance GetFuseContext DeviceCall where 
    fuseUID = getFuseUID
    fuseGID = getFuseGID

instance WithCurrentDevice DeviceCall where
    currentDevice = ask
    callCurrentDevice args = 
        do device <- ask
           liftAdbCall $ Adb.callForDevice device args

listDevices :: (MonadIO m) => m (Either String [Device])
listDevices = liftIO $ runExceptT Adb.listDevices

liftAdbCall :: (CanFail o, MonadIO o) => ExceptT String IO a -> o a
liftAdbCall action = do
  value <- liftIO $ runExceptT action
  case value of
    Right r -> return r
    -- TODO: logs?
    Left e -> simpleError eINVAL
  

instance Logging DeviceCall where
    writeLog = writeLogWriter

instance Logging AdbFsCall where
    writeLog = writeLogWriter

writeLogWriter :: (MonadWriter [String] m) => String -> m ()
writeLogWriter l = tell [l]

-- instance (Monad m,) => Logging m where
--     writeLog l = tell [l]

withCleanupError :: (MonadError Error m) => m a -> m () -> m a
withCleanupError action cleanup =
    action `catchError` (\e -> do 
                           cleanup
                           throwError e)

instance CanFail DeviceCall where
    failWith = throwError
    withCleanup = withCleanupError

instance CanFail AdbFsCall where
    failWith = throwError
    withCleanup = withCleanupError

onDevice :: DeviceId -> DeviceCall a -> AdbFsCall a
onDevice deviceId action = findDevice deviceId >>= runReaderT action 

simpleError :: (CanFail m) => Errno -> m a
simpleError code = failWith $ Error code 

deviceCallInIO :: Device -> DeviceCall a -> IO (Either Error a, [String])
deviceCallInIO device action = 
    runWriterT $ runExceptT $ runReaderT action device

data LogLevel = LogSilent
              | LogFailsOnly
              | LogFull

data Option = LogLevel (Maybe LogLevel)
            | LoggingFile String

type LogFunction = Either String String -> [String] -> IO ()

msgError :: (CanFail m, Logging m) => Errno -> String -> m a
msgError code msg = do 
  log $ "error: " ++ msg
  failWith $ Error code

data FsEntry = FsEntry { fseOpen             :: OpenMode -> OpenFileFlags -> AdbFsCall FileHandle
                       , fseRead             :: FileHandle -> ByteCount -> FileOffset -> AdbFsCall B.ByteString
                       , fseWrite            :: FileHandle -> B.ByteString -> FileOffset -> AdbFsCall ByteCount
                       , fseGetFileStat      :: AdbFsCall FileStat
                       , fseOpenDirectory    :: AdbFsCall Errno
                       , fseReadDirectory    :: AdbFsCall FilesList
                       , fseSetFileMode      :: FileMode -> AdbFsCall Errno
                       , fseSetOwnerAndGroup :: UserID -> GroupID -> AdbFsCall Errno
                       , fseReadSymlink      :: AdbFsCall FilePath
                       , fseSetFileSize      :: FileOffset -> AdbFsCall Errno
                       , fseCreateDevice     :: EntryType -> FileMode -> DeviceID -> AdbFsCall Errno
                       , fseCreateDirectory  :: AdbFsCall Errno
                       , fseRemoveLink       :: AdbFsCall Errno
                       , fseRemoveDirectory  :: AdbFsCall Errno}

data DeviceShellCall = DeviceShellCall [String]
                       deriving (Show, Eq)

log :: Logging m => String -> m ()
log l = writeLog l

logLn :: Logging m => String -> m ()
logLn ln = writeLog $ ln ++ "\n"

instance Show OpenMode where
    show ReadOnly = "ro"
    show WriteOnly = "wo"
    show ReadWrite = "rw"

instance Show OpenFileFlags where
    show (OpenFileFlags append exclusive noccty nonblock trunc) =
        intercalate "-" $ map (\(b, s) -> if b then s else "|") [(append, "append")
                                                                , (exclusive, "exclusive")
                                                                , (noccty, "noccty")
                                                                , (nonblock, "nonblock")
                                                                , (trunc, "trunc")]

defaultFsEntry :: FsEntry
defaultFsEntry = FsEntry { fseOpen             = const $ const $ noImpl
                         , fseRead             = const $ const $ const $ noImpl
                         , fseWrite            = const $ const $ const $ noImpl
                         , fseGetFileStat      = noImpl
                         , fseOpenDirectory    = noImpl
                         , fseReadDirectory    = noImpl
                         , fseReadSymlink      = noImpl
                         , fseSetFileMode      = const $ noImpl
                         , fseSetOwnerAndGroup = const $ const $ noImpl
                         , fseSetFileSize      = const $ noImpl
                         , fseCreateDevice     = const $ const $ const $ noImpl 
                         , fseCreateDirectory  = noImpl
                         , fseRemoveDirectory  = noImpl
                         , fseRemoveLink       = noImpl}
    where noImpl = simpleError eNOSYS

adbFSOps :: LogFunction -> FuseOperations FileHandle
adbFSOps logFunc =
    defaultFuseOps { fuseGetFileStat = \path ->  
                                       run forResult $ method "fuseGetFileStat"
                                                >> path `as` "path"
                                                >> (fseGetFileStat $ pathToFsEntry path)

                    , fuseOpen = \path -> \mode -> \flags ->
                                   run forResult $ method "fuseOpen"
                                           >> path `as` "path" >> mode `as` "mode" >> flags `as` "flags" 
                                           >> (fseOpen (pathToFsEntry path) mode flags)

                    , fuseRead = \path -> \handle -> \count -> \offset -> 
                                 run forResult $ method "fuseRead" 
                                         >> path `as` "path" >> handle `as` "handle" >> count `as` "count" >> offset `as` "offset" 
                                         >> (fseRead (pathToFsEntry path) handle count offset)

                    , fuseWrite = \path -> \handle -> \bytes -> \offset ->
                                  run forResult $ method "fuseWrite"
                                          >> path `as` "path" >> handle `as` "handle" >> bytes `as` "bytes" >> offset `as` "offset"
                                          >> (fseWrite (pathToFsEntry path) handle bytes offset)

                    , fuseOpenDirectory = \path -> 
                                          run forCode $ method "fuseOpenDirectory"
                                                  >> path `as` "path" 
                                                  >> (fseOpenDirectory $ pathToFsEntry path)
                    , fuseReadDirectory = \path ->
                                          run forResult $ method "fuseReadDirectory" 
                                                  >> path `as` "path"
                                                  >> (fseReadDirectory $ pathToFsEntry path)
                    , fuseGetFileSystemStats = adbFsGetFileSystemStats
                    , fuseReadSymbolicLink = \path -> 
                                             run forResult $ method "fuseReadSymbolicLink" 
                                                     >> path `as` "path"
                                                     >> (fseReadSymlink $ pathToFsEntry path)
                    , fuseSetFileSize = \path -> \size ->
                                        run forCode $ method "fuseSetFileSize" 
                                                >> path `as` "path" >> size `as` "size"
                                                >> (fseSetFileSize (pathToFsEntry path) size)

                    , fuseSetFileMode = \path -> \fileMode ->
                                        run forCode $ method "fuseSetFileMode"
                                                >> path `as` "path" >> fileMode `as` "fileMode"
                                                >> (fseSetFileMode (pathToFsEntry path) fileMode)

                    , fuseSetOwnerAndGroup = \path -> \userId -> \groupId ->
                                        run forCode $ method "fuseSetOwnerAndGroup"
                                                >> path `as` "path" >> userId `as` "usedId" >> groupId `as` "groupId"
                                                >> (fseSetOwnerAndGroup (pathToFsEntry path) userId groupId)

                    , fuseCreateDevice = \path -> \entryType -> \fileMode -> \deviceId -> 
                                         run forCode $ method "fuseCreateDevice"
                                                 >> path `as` "path" >> entryType `as` "entryType" >> fileMode `as` "fileMode" >> deviceId `as` "deviceId"
                                                 >> (fseCreateDevice (pathToFsEntry path) entryType fileMode deviceId)
                    , fuseRemoveLink = \path -> 
                                       run forCode $ method "fuseRemoveLink"
                                           >> path `as` path 
                                           >> (fseRemoveLink (pathToFsEntry path))

                    , fuseRemoveDirectory = \path -> 
                                       run forCode $ method "fuseRemoveDirectory"
                                           >> path `as` path 
                                           >> (fseRemoveDirectory (pathToFsEntry path))

                    , fuseCreateDirectory = \path -> \mode ->
                                       run forCode $ method "fuseCreateDirectory"
                                           >> path `as` "path" >> mode `as` "mode"
                                           >> (fseCreateDirectory (pathToFsEntry path))

                   }
        where method name = logLn $ "called method: " ++ name
              a `as` n = logLn $ " param: " ++ n ++ ": " ++ (show a)

              run :: (Show a) => (Either Error a -> e) -> AdbFsCall a -> IO e
              run convert action = do 
                (result, logged) <- runWriterT $ runExceptT action

                let resultMsg = case result of 
                                  Right result -> Right $ "call ok: " ++ (show result)
                                  Left fail -> Left $ "call failed: " ++ (show fail)

                logFunc resultMsg logged

                return $ convert result

              forCode = either eErrno id 
              forResult = either (Left . eErrno) Right 


dirStat :: (GetFuseContext m) => m FileStat
dirStat = do 
  uid <- fuseUID
  gid <- fuseGID
  
  return $ FileStat { statEntryType = Directory
                    , statFileMode = mconcat
                                     [ ownerReadMode
                                     , ownerExecuteMode
                                     , groupReadMode
                                     , groupExecuteMode
                                     , otherReadMode
                                     , otherExecuteMode
                                     ]
                    , statLinkCount = 2
                    , statFileOwner = uid
                    , statFileGroup = gid
                    , statSpecialDeviceID = 0
                    , statFileSize = 4096
                    , statBlocks = 1
                    , statAccessTime = 0
                    , statModificationTime = 0
                    , statStatusChangeTime = 0}

emptyDirContents :: GetFuseContext m => m FilesList
emptyDirContents = dirsFromNames [".", ".."]

instance (Monoid (AdbFsCall FilesList)) where
    mempty = return $ []
    mappend l r = (++) <$> r  <*> l

rootEntry = defaultFsEntry { fseGetFileStat = dirStat
                           , fseOpenDirectory = return eOK
                           , fseReadDirectory = emptyDirContents `mappend` dirsFromDevices }

deviceFsEntry deviceId path
    = defaultFsEntry { fseOpen = const $ const $ return (deviceId, path)
                     , fseRead = \handle -> \count -> \offset -> onDevice deviceId $ deviceRead path count offset
                     , fseWrite = \handle -> \bytes -> \offset -> onDevice deviceId $ deviceWrite path bytes offset
                     , fseGetFileStat = onDevice deviceId $ deviceStat path
                     , fseOpenDirectory = return eOK
                     , fseReadDirectory = emptyDirContents `mappend` (onDevice deviceId $ deviceLs path)
                     , fseSetFileSize = \size -> onDevice deviceId $ deviceSetFileSize path size
                     , fseReadSymlink = onDevice deviceId $ deviceReadLink path
                     , fseSetFileMode = \mode -> return eOK
                     , fseCreateDevice = \entry -> \mode -> \id -> onDevice deviceId $ deviceCreateDevice path entry id
                     , fseCreateDirectory = onDevice deviceId $ deviceCreateDirectory path
                     , fseRemoveLink = onDevice deviceId $ deviceDeleteFile path
                     , fseRemoveDirectory = onDevice deviceId $ deviceDeleteDir path }


deviceRootEntry = defaultFsEntry { fseGetFileStat = dirStat
                                 , fseOpenDirectory = return eOK
                                 , fseReadDirectory = emptyDirContents `mappend` dirsOfDevicePseudoFs }

dirsFromNames :: GetFuseContext m => [String] -> m FilesList
dirsFromNames names = do 
  ds <- dirStat
  return $ map ((\n -> (n, ds))) names

dirsOfDevicePseudoFs :: AdbFsCall FilesList
dirsOfDevicePseudoFs = dirsFromNames ["fs"]

randomFileIn :: FilePath -> IO FilePath
randomFileIn path = do
  postfix <- randomRIO ((10000000000000000000, 100000000000000000000) :: (Integer, Integer))
  return $ path </> ("adb_fuse_transport." ++ show (postfix))

dirsFromDevices :: AdbFsCall FilesList
dirsFromDevices = do
  devices <- (either (const []) id) <$> listDevices
  dirsFromNames $ map serialNo devices

qualifyPath :: String -> Maybe PathQualification
qualifyPath path = either (const Nothing) Just $ P.parse P.adbFsPath path



pathToFsEntry :: String -> FsEntry
pathToFsEntry path =
    case qualifyPath path of
      Nothing -> defaultFsEntry
      Just (FsRoot) -> rootEntry
      Just (Device deviceName) -> deviceRootEntry
      Just (DeviceFs deviceName) -> deviceFsEntry deviceName ""
      Just (InDeviceFs deviceName innerPath) -> deviceFsEntry deviceName innerPath


fsBlockSize :: Int
fsBlockSize = 1024 * 50

blockify :: ByteCount -> FileOffset -> U.Block
blockify = U.blockify $ fsBlockSize

adbFsGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
adbFsGetFileSystemStats str =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = fromIntegral fsBlockSize
    , fsStatBlockCount = 1000000
    , fsStatBlocksFree = 1000000
    , fsStatBlocksAvailable = 1000000
    , fsStatFileCount = 500
    , fsStatFilesFree = 1000
    , fsStatMaxNameLength = 255
    }


data LsError = PermissionDenied String

findDevice :: DeviceId -> AdbFsCall Device
findDevice deviceId = do
  devicesResponse <- listDevices
  case devicesResponse of 
    Left error -> msgError eNOENT error
    Right devices -> 
        case find ((deviceId ==) . serialNo) devices of 
          Just device -> return device
          Nothing -> msgError eNOENT $ "No deivce: " ++ deviceId

quoteAdbCallArgs :: [String] -> [String]
quoteAdbCallArgs = map (foldr quote "")
    where quote char acc 
                | char `elem` quotable = '\\' : d
                | otherwise = d
                where d = char : acc
          quotable = ['\'', '"', ' ']

formatAdbShellCall :: [String] -> Char -> Char -> [String]
formatAdbShellCall inArgs okMarker failMarker
    = quoteAdbCallArgs $ "shell" : (["(", "("] ++ inArgs ++ [")", "&&", echo okMarker, ")", "||", echo failMarker]) 
    where echo m = "echo -n " ++ [m]
    
deviceShellCall :: (Logging m, WithCurrentDevice m) => DeviceShellCall -> m (Either String String)
deviceShellCall (DeviceShellCall inArgs) = do
  device <- currentDevice

  let args = formatAdbShellCall inArgs ok fail
      ok = 't'
      fail = 'f'

  logLn $ "adb (" ++ (serialNo device) ++ " ) shell call: " ++ (show args)

  rawResponse <- callCurrentDevice args

  logLn $ "response: " ++ rawResponse

  let (response, marker) = splitAt ((length rawResponse) - 1) rawResponse
      resultType = if marker == [ok]
                   then Right
                   else Left 
                     
  return $ resultType response

parseWith :: Parser a -> String -> DeviceCall a
parseWith parser string = case P.parse parser string of
                            Left err -> do 
                              logLn $ "can't be parsed: " ++ (show err)
                              simpleError eINVAL
                            Right result -> return result

mapLeft :: (l -> nl) -> Either l r  -> Either nl r
mapLeft f (Right value) = Right value
mapLeft f (Left  error) = Left $ f error


failOnLeft :: (CanFail m, Logging m) => Either String a -> m a
failOnLeft (Right value) = return value
failOnLeft (Left fail) = logLn fail >> simpleError eINVAL
  

deviceCall :: (CanFail m, Logging m, WithCurrentDevice m) => [String] -> Parser a -> m a
deviceCall args parser = do

  response <- callCurrentDevice $ quoteAdbCallArgs args
  device <- currentDevice

  logLn $ "adb (" ++ (serialNo device) ++ " ) call: " ++ (show args)
  logLn $ "response: " ++ response

  failOnLeft $ mapLeft (("response can't be parsed:" ++ ) . show) $ P.parse parser response

deviceLs :: String -> DeviceCall FilesList
deviceLs path = (deviceCall ["shell", "ls", "-al", path ++ "/"] $ P.rfseFromAdbLs) >>= mapM statFromRemoteFsEntry 


deviceReadLink :: FilePath -> DeviceCall FilePath
deviceReadLink path = deviceCall ["shell", "realpath", "/" ++ path] $ upToRoot <$> P.filePathFromRealpathResponse
    where upToRoot innerPath@(firstChar:_) = if firstChar == '/'
                                             then pathRelativeToRoot innerPath
                                             else innerPath
          pathRelativeToRoot p = "." ++ (concat (take ((length $ splitPath path) - 2) $ repeat "/..")) ++ p

deviceStat :: (WithCurrentDevice m, Logging m, CanFail m, GetFuseContext m) => FilePath -> m FileStat
deviceStat path = do

  let args = ["shell", "ls", "-ald", "/" ++ path]

  statResult <- deviceCall args P.singleFileStat

  case statResult of
    Just s -> snd <$> statFromRemoteFsEntry s
    Nothing -> simpleError eNOENT

withTempFile :: (CanFail m, MonadIO m) => FilePath -> (FilePath -> m ()) -> (FilePath -> m a) -> m a
withTempFile prefix delete action = do
  tempFile <- liftIO $ randomFileIn prefix

  (action tempFile) `withCleanup` (delete tempFile)

  -- let performAndClean = do
  --       result <- action tempFile
  --       delete tempFile
  --       return result

  --     cleanupAndRethrow e = do
  --       delete tempFile
  --       throwError e

  -- performAndClean `catchError` cleanupAndRethrow

withLocalTempFile :: (CanFail m, MonadIO m) => (FilePath -> m a) -> m a
withLocalTempFile = withTempFile "/tmp" (liftIO . removeFile)

withRemoteTempFile :: (WithCurrentDevice m, Logging m, CanFail m, MonadIO m) => (FilePath -> m a) -> m a
withRemoteTempFile action = withTempFile "/sdcard" remoteDelete action

remoteDelete :: (WithCurrentDevice m, CanFail m, Logging m) => FilePath -> m ()
remoteDelete filePath = deviceCall ["shell", "rm", "-f", filePath] P.emptyResponse

ddCommand :: FilePath -> Maybe FilePath -> U.Block -> [String] 
ddCommand iF oF (U.Block { U.blckFirstBlock = firstBlock
                         , U.blckBlocksCount = blocksCount
                         , U.blckBlockSize = blockSize } )
                    = ([ "shell"
                       , "dd"
                       , "if=" ++ iF
                       , "bs=" ++ (show blockSize)
                       , "skip=" ++ (show firstBlock)
                       , "count=" ++ (show blocksCount)]
                       ++ ofParam)
    where ofParam = fromMaybe [] $ fmap (\x -> ["of=" ++ x]) oF

pullToTempFile :: (WithCurrentDevice m, CanFail m, MonadIO m, Logging m) => FilePath -> (FilePath -> m a) -> m a
pullToTempFile remoteFilePath action = 
  withLocalTempFile $ \tempFilePath ->
      do
        -- FIXME: response not parsed
        deviceCall ["pull", remoteFilePath, tempFilePath] P.acceptAnything

        action tempFilePath

deviceRead :: (WithCurrentDevice m, CanFail m, MonadIO m, Logging m) => FilePath -> ByteCount -> FileOffset -> m B.ByteString
deviceRead path count offset = do
  let block@U.Block { U.blckFirstBlock = firstBlock
                    , U.blckBlocksCount = blocksCount
                    , U.blckOffsetInFirstBlock = skipInFirstBlock } 
          = blockify count offset

      blockToResult = (B.take (fromIntegral count)) . (B.drop skipInFirstBlock) . B.pack

  blockToResult <$> deviceCall (ddCommand path Nothing block) P.parseDDReadFile

  -- withRemoteTempFile $ \onDeviceTempFile ->
  --     do

  --       -- FIXME: response not parsed
  --       deviceCall (ddCommand path onDeviceTempFile block) P.acceptAnything

  --       pullToTempFile onDeviceTempFile $ \localTempFile -> 
  --           do
  --             d <- liftIO $ B.readFile localTempFile

  --             return $ B.take (fromIntegral count) $ B.drop skipInFirstBlock d

deviceWrite :: FilePath -> B.ByteString -> FileOffset -> DeviceCall ByteCount
deviceWrite targetPath dataToWrite offset = do
  -- at first we need to get original block containing the data to be
  -- written, as we can only "dd" data to file
  let block@U.Block { U.blckFirstBlock = firstBlock
                    , U.blckBlocksCount = blocksCount
                    , U.blckOffsetInFirstBlock = inBlockOffset
                    , U.blckBlockSize = blockSize } 
          = blockify (fromIntegral dataSize) offset

      dataSize = B.length dataToWrite

  originalBlock <- deviceRead targetPath (fromIntegral (blocksCount * blockSize)) (fromIntegral (firstBlock * blockSize))
  
  let transformedBlock = B.concat [ B.take inBlockOffset originalBlock
                                  , dataToWrite
                                  , B.drop (inBlockOffset + dataSize) originalBlock]
            
  withLocalTempFile $ \localPath -> 
      withRemoteTempFile $ \remotePath -> do
                 liftIO $ B.writeFile localPath transformedBlock

                 deviceCall ["push", localPath, remotePath] P.emptyResponse

                 deviceCall (ddCommand remotePath (Just targetPath) block) $ P.acceptAnything

  return $ fromIntegral dataSize



deviceSetFileSize :: FilePath -> FileOffset -> DeviceCall Errno
deviceSetFileSize path 0 = do 
  deviceCall ["shell", "dd", "of=" ++ path, "count=0"] P.acceptAnything
  return eOK

deviceSetFileSize _ _ = return eINVAL

deviceCreateDevice :: FilePath -> EntryType -> DeviceID -> DeviceCall Errno
deviceCreateDevice path RegularFile _ = do
  deviceCall ["shell", "touch", path] P.emptyResponse
  return eOK

deviceCreateDevice _ _ _= simpleError eINVAL

deviceCreateDirectory :: FilePath -> DeviceCall Errno
deviceCreateDirectory path = do
  deviceCall ["shell", "mkdir", path] P.emptyResponse
  return eOK


deviceDeleteFile :: FilePath -> DeviceCall Errno
deviceDeleteFile path = do
  deviceCall ["shell", "rm", "-f", path] P.emptyResponse
  return eOK

deviceDeleteDir :: FilePath -> DeviceCall Errno
deviceDeleteDir path = do
  deviceCall ["shell", "rm", "-fd", path] P.emptyResponse
  return eOK

isFileMode :: FileMode -> FileMode -> Bool
isFileMode whatToCheck modeOfQuestion = (whatToCheck `intersectFileModes` fileTypeModes) == modeOfQuestion


statFromRemoteFsEntry :: (GetFuseContext m) => RemoteFsEntry -> m (FilePath, FileStat)
statFromRemoteFsEntry (RemoteFsEntry fileMode size name) = do
  uid <- fuseUID
  gid <- fuseGID

  return (name, 
          FileStat { statEntryType = fromMaybe RegularFile $ snd <$> (find ((fileMode `isFileMode`) . fst)
                                                                      [(blockSpecialMode, BlockSpecial)
                                                                       , (characterSpecialMode, CharacterSpecial)
                                                                       , (namedPipeMode, NamedPipe)
                                                                       , (directoryMode, Directory)
                                                                       , (symbolicLinkMode, SymbolicLink)
                                                                       , (socketMode, Socket)])
                   , statFileMode = fileMode
                   , statLinkCount = 2
                   , statFileOwner = uid
                   , statFileGroup = gid
                   , statSpecialDeviceID = 0
                   , statFileSize = fromIntegral size
                   , statBlocks = 1
                   , statAccessTime = 0
                   , statModificationTime = 0
                   , statStatusChangeTime = 0})


