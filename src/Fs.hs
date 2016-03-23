{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-} 
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Fs (adbFSOps, LogFunction) where

import System.Fuse

import qualified Data.ByteString.Char8 as B
import Adb (serialNo, runAdbIO, listDevices, ifAdbPresent, Device)
import qualified Adb as Adb

import qualified System.Environment as Env
import System.Directory (removeFile)
import System.Random (randomRIO)
import Control.Monad (forM, void)
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Applicative ((<*))

import System.FilePath

import System.Console.GetOpt

import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO

import qualified Text.Parsec as P
import Text.Parsec (oneOf, count, anyChar, digit, manyTill, space, spaces, many1, choice, try, char, string, many, eof, noneOf)
import Data.Maybe (fromMaybe, isJust)
import Data.List (find, intercalate)

import Prelude hiding (log)

data Error = Error { eErrno ::  Errno }
           deriving (Show, Eq)

type FileHandle = (DeviceId, FilePath)

type DeviceId = String
type FsCall a = IO (Either Error a)
type FilesList = [(FilePath, FileStat)]

type AdbFsCall = ExceptT Error (WriterT [String] IO)

type DeviceCall = ReaderT Device AdbFsCall



instance Monoid FileMode where
    mempty = nullFileMode
    mappend = unionFileModes

onDevice :: DeviceId -> DeviceCall a -> AdbFsCall a
onDevice deviceId action = findDevice deviceId >>= runReaderT action 

simpleError :: (MonadError Error m) => Errno -> m a
simpleError code = throwError $ Error code 

data LogLevel = LogSilent
              | LogFailsOnly
              | LogFull

data Option = LogLevel (Maybe LogLevel)
            | LoggingFile String

type LogFunction = Either String String -> [String] -> IO ()

msgError :: (MonadError Error m, MonadWriter [String] m) => Errno -> String -> m a
msgError code msg = do 
  log $ "error: " ++ msg
  throwError $ Error code

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

log :: MonadWriter [String] m => String -> m ()
log ln = tell [ln]

logLn :: MonadWriter [String] m => String -> m ()
logLn ln = tell [ln ++ "\n"]

instance Show Errno where
    show (Errno c) = show c

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

blockSize :: Int
blockSize = 1024 * 50

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


dirStat :: FuseContext -> FileStat
dirStat ctx = FileStat { statEntryType = Directory
                       , statFileMode = mconcat
                                          [ ownerReadMode
                                          , ownerExecuteMode
                                          , groupReadMode
                                          , groupExecuteMode
                                          , otherReadMode
                                          , otherExecuteMode
                                          ]
                       , statLinkCount = 2
                       , statFileOwner = fuseCtxUserID ctx
                       , statFileGroup = fuseCtxGroupID ctx
                       , statSpecialDeviceID = 0
                       , statFileSize = 4096
                       , statBlocks = 1
                       , statAccessTime = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0}




emptyDirContents :: AdbFsCall FilesList
emptyDirContents = dirsFromNames [".", ".."]

instance (Monoid (AdbFsCall FilesList)) where
    mempty = return $ []
    mappend l r = (++) <$> r  <*> l

getDirStat :: AdbFsCall FileStat
getDirStat = do
  ctx <- liftIO $ getFuseContext
  return $ dirStat ctx

rootEntry = defaultFsEntry { fseGetFileStat = getDirStat
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


deviceRootEntry = defaultFsEntry { fseGetFileStat = getDirStat
                                 , fseOpenDirectory = return eOK
                                 , fseReadDirectory = emptyDirContents `mappend` dirsOfDevicePseudoFs }

dirsFromNames :: [String] -> AdbFsCall FilesList
dirsFromNames names = do
  ctx <- liftIO $ getFuseContext
  return $ map ((\n -> (n, dirStat ctx))) names

dirsOfDevicePseudoFs :: AdbFsCall FilesList
dirsOfDevicePseudoFs = dirsFromNames ["fs"]

randomFileIn :: FilePath -> IO FilePath
randomFileIn path = do
  postfix <- randomRIO ((10000000000000000000, 100000000000000000000) :: (Integer, Integer))
  return $ path </> ("adb_fuse_transport." ++ show (postfix))

dirsFromDevices :: AdbFsCall FilesList
dirsFromDevices = do
  devices <- (either (const []) id) <$> (liftIO $ runAdbIO $ listDevices)
  dirsFromNames $ map serialNo devices

data PathQualification = FsRoot
                       | Device String
                       | DeviceFs String
                       | InDeviceFs String String
                         deriving (Show, Eq)

qualifyPath :: String -> Maybe PathQualification
qualifyPath path = either (const Nothing) Just $ P.parse parser "" path
    where parser = do
            let pathSep = char '/'
                v `ifEndOr` otherwise = choice [try $ eof >> return v,
                                                otherwise]

                nextPart = many $ noneOf "/"

            pathSep

            FsRoot `ifEndOr` do
                     deviceName <- nextPart

                     (Device deviceName) `ifEndOr` do
                               pathSep
                               subdevice <- nextPart

                               if subdevice == "fs"

                               then (DeviceFs deviceName) `ifEndOr` ((InDeviceFs deviceName) <$> (many $ anyChar))

                               else error "unknown"



pathToFsEntry :: String -> FsEntry
pathToFsEntry path =
    case qualifyPath path of
      Nothing -> defaultFsEntry
      Just (FsRoot) -> rootEntry
      Just (Device deviceName) -> deviceRootEntry
      Just (DeviceFs deviceName) -> deviceFsEntry deviceName ""
      Just (InDeviceFs deviceName innerPath) -> deviceFsEntry deviceName innerPath


adbFsGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
adbFsGetFileSystemStats str =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = fromIntegral blockSize
    , fsStatBlockCount = 1000000
    , fsStatBlocksFree = 1000000
    , fsStatBlocksAvailable = 1000000
    , fsStatFileCount = 500
    , fsStatFilesFree = 1000
    , fsStatMaxNameLength = 255
    }

type Parser = P.Parsec String ()

data LsError = PermissionDenied String

data RemoteFsEntry = RemoteFsEntry { rfseMode :: FileMode
                                   , rfseSize :: Integer
                                   , rfseName :: String }
                   deriving (Show, Eq)

parseFileModeRWXFormat :: Parser FileMode
parseFileModeRWXFormat = mconcat <$> modes
    where modes = forM format $ \alternatives -> do
                    c <- anyChar
                    return $ fromMaybe mempty $ lookup c alternatives

          format = [[('d', directoryMode),
                     ('l', symbolicLinkMode)],

                    [('r', ownerReadMode)],
                    [('w', ownerWriteMode)],
                    [('x', ownerExecuteMode)],
                    [('r', groupReadMode)],
                    [('w', groupWriteMode)],
                    [('x', groupExecuteMode)],
                    [('r', otherReadMode)],
                    [('w', otherWriteMode)],
                    [('x', otherExecuteMode)]]

newline :: Parser String
newline = choice [try $ string "\r\n",
                  try $ string "\n",
                  string "\r"]

tillEndOfLine :: Parser String
tillEndOfLine = anyChar `manyTill` newline

parseAdbLsLine :: Parser RemoteFsEntry
parseAdbLsLine = do
  mode <- parseFileModeRWXFormat
  -- skip group
  spaces
  manyTill anyChar space
  -- skip user
  spaces
  manyTill anyChar space
  spaces

  let parseDate = (num 4) >> char '-' >> (num 2) >> char '-' >> (num 2) >> char ' ' >> (num 2) >> char ':' >> (num 2)
      num cnt = count cnt digit
      parseName = if intersectFileModes mode symbolicLinkMode /= nullFileMode
                  then anyChar `manyTill` (string " -> ") <* anyChar `manyTill` newline
                  else anyChar `manyTill` newline

  (size, name) <- choice [try $ do
                            size <- read <$> many1 digit
                            spaces
                            parseDate
                            space
                            name <- parseName
                            return (size, name),
                          do
                            parseDate
                            space
                            name <- parseName
                            return (0, name)]


  return $ RemoteFsEntry mode size name

permissionDeniedItem :: Parser RemoteFsEntry
permissionDeniedItem = do 
  anyChar `manyTill` char '\''
  name <- anyChar `manyTill` (try $ string "' failed: Permission denied")
  newline 

  return $ RemoteFsEntry nullFileMode 0 $ takeFileName name

parseAdbLs :: Parser [RemoteFsEntry]
parseAdbLs = many $ choice [try $ parseAdbLsLine,
                            permissionDeniedItem]
            

findDevice :: DeviceId -> AdbFsCall Device
findDevice deviceId = do
  devicesResponse <- (liftIO $ runAdbIO listDevices)
  case devicesResponse of 
    Left error -> msgError eNOENT error
    Right devices -> 
        case find ((deviceId ==) . serialNo) devices of 
          Just device -> return device
          Nothing -> msgError eNOENT $ "No deivce: " ++ deviceId

deviceShellCall :: [String] -> DeviceCall (Either String String)
deviceShellCall inArgs = do
  device <- ask

  let args = ["shell"] ++ (map quote $ ["(", "("] ++ inArgs ++ [")", "&&", echo ok, ")", "||", echo fail])
      quote (c:xs) = if c `elem` quotable
                     then '\\' : c : rest
                     else c : rest
          where rest = quote xs
      quote [] = []
      quotable = ['\'', '"']
      echo m = "echo -n " ++ [m]
      fail = 'f'
      ok = 't'

  logLn $ "adb (" ++ (serialNo device) ++ " ) shell call: " ++ (show args)

  rawResponse <- liftIO $ runAdbIO $ Adb.callForDevice device args

  logLn $ "response: " ++ rawResponse

  let (response, marker) = splitAt ((length rawResponse) - 1) rawResponse
      resultType = if marker == [ok]
                   then Right
                   else Left 
                     
  return $ resultType response

parseWith :: Parser a -> String -> DeviceCall a
parseWith parser string = case P.parse parser "" string of
                            Left err -> do 
                              logLn $ "can't be parsed: " ++ (show err)
                              simpleError eINVAL
                            Right result -> return result

deviceCall :: [String] -> Parser a -> DeviceCall a
deviceCall args parser = do
  device <- ask

  response <- liftIO $ runAdbIO $ Adb.callForDevice device args

  logLn $ "adb (" ++ (serialNo device) ++ " ) call: " ++ (show args)
  logLn $ "response: " ++ response

  case P.parse parser "" response of
    Left err -> do 
      logLn $ "response can't be parsed: " ++ (show err)
      simpleError eINVAL

    Right contents -> return contents



emptyResponse :: Parser ()
emptyResponse = eof

acceptAnything :: Parser ()
acceptAnything = return ()

deviceLs :: String -> DeviceCall FilesList
deviceLs path = do 
  ctx <- liftIO $ getFuseContext

  let filesFromRemoteEntries = map (statFromRemoteFsEntry ctx)

  deviceCall ["shell", "ls", "-al", path ++ "/"] $ filesFromRemoteEntries <$> parseAdbLs


deviceReadLink :: FilePath -> DeviceCall FilePath
deviceReadLink path = deviceCall ["shell", "realpath", "/" ++ path] $ upToRoot <$> (anyChar `manyTill` newline)
    where upToRoot innerPath@(firstChar:_) = if firstChar == '/'
                                             then pathRelativeToRoot innerPath
                                             else innerPath
          pathRelativeToRoot p = "." ++ (concat (take ((length $ splitPath path) - 2) $ repeat "/..")) ++ p

deviceStat :: FilePath -> DeviceCall FileStat
deviceStat path = do
  ctx <- liftIO getFuseContext

  let parseResponse = choice [Just <$> try parseAdbLsLine
                             , Just <$> try permissionDeniedItem
                             , parseNotFound >> return Nothing]

      parseNotFound = anyChar `manyTill` (string ": No such file or directory")

      toFileStat = snd . statFromRemoteFsEntry ctx

      args = ["shell", "ls", "-ald", "/" ++ path]

  statResult <- deviceCall args parseResponse

  case statResult of
    Just s -> return $ toFileStat s
    Nothing -> simpleError eNOENT

toBlockParams :: ByteCount -> FileOffset -> (Int, Int, Int)
toBlockParams count inOffset = (firstBlock, blocksCount, inBlockOffset)
    where offset = (fromIntegral inOffset)
          firstBlock = offset `div` blockSize
          inBlockOffset = offset `mod` blockSize
          blocksCount = (((fromIntegral count) + inBlockOffset) `div` blockSize) + 1


withTempFile :: (MonadError Error m, MonadIO m) => FilePath -> (FilePath -> m ()) -> (FilePath -> m a) -> m a
withTempFile prefix delete action = do
  tempFile <- liftIO $ randomFileIn prefix

  let performAndClean = do
        result <- action tempFile
        delete tempFile
        return result

      cleanupAndRethrow e = do
        delete tempFile
        throwError e

  performAndClean `catchError` cleanupAndRethrow

withLocalTempFile :: (MonadError Error m, MonadIO m) => (FilePath -> m a) -> m a
withLocalTempFile = withTempFile "/tmp" (liftIO . removeFile)

withRemoteTempFile :: (FilePath -> DeviceCall a) -> DeviceCall a
withRemoteTempFile action = do
  device <- ask

  withTempFile "/sdcard" remoteDelete action

remoteDelete :: FilePath -> DeviceCall ()
remoteDelete filePath = deviceCall ["shell", "rm", "-f", filePath] emptyResponse

ddCommand :: FilePath -> FilePath -> Int -> Int -> Int -> [String] 
ddCommand iF oF skipInput skipOutput count = ["shell"
                                             , "dd"
                                             , "if=" ++ iF
                                             , "of=" ++ oF
                                             , "bs=" ++ (show blockSize)
                                             , "skip=" ++ (show skipInput)
                                             , "seek=" ++ (show skipOutput)
                                             , "count=" ++ (show count)]

pullToTempFile :: FilePath -> (FilePath -> DeviceCall a) -> DeviceCall a
pullToTempFile remoteFilePath action = 
  withLocalTempFile $ \tempFilePath ->
      do
        -- FIXME: response not parsed
        deviceCall ["pull", remoteFilePath, tempFilePath] acceptAnything

        action tempFilePath

deviceRead :: FilePath -> ByteCount -> FileOffset -> DeviceCall B.ByteString
deviceRead path count offset = do
  let (firstBlock, blocksCount, skipInFirstBlock) = toBlockParams count offset

  withRemoteTempFile $ \onDeviceTempFile ->
      do

        -- FIXME: response not parsed
        deviceCall (ddCommand path onDeviceTempFile firstBlock 0 blocksCount) acceptAnything

        pullToTempFile onDeviceTempFile $ \localTempFile -> 
            do
              d <- liftIO $ B.readFile localTempFile

              return $ B.take (fromIntegral count) $ B.drop skipInFirstBlock d

deviceWrite :: FilePath -> B.ByteString -> FileOffset -> DeviceCall ByteCount
deviceWrite targetPath dataToWrite offset = do
  -- at first we need to get original block containing the data to be
  -- written, as we can only "dd" data to file
  let blockInfo@(firstBlock, blocksCount, inBlockOffset) = toBlockParams (fromIntegral dataSize) offset
      dataSize = B.length dataToWrite

  originalBlock <- deviceRead targetPath (fromIntegral (blocksCount * blockSize)) (fromIntegral (firstBlock * blockSize))
  
  let transformedBlock = B.concat [ B.take inBlockOffset originalBlock
                                  , dataToWrite
                                  , B.drop (inBlockOffset + dataSize) originalBlock]
            
  withLocalTempFile $ \localPath -> 
      withRemoteTempFile $ \remotePath -> do
                 liftIO $ B.writeFile localPath transformedBlock

                 deviceCall ["push", localPath, remotePath] eof

                 deviceCall (ddCommand remotePath targetPath 0 firstBlock blocksCount) $ many anyChar

  return $ fromIntegral dataSize



deviceSetFileSize :: FilePath -> FileOffset -> DeviceCall Errno
deviceSetFileSize path 0 = do 
  deviceCall ["shell", "dd", "of=" ++ path, "count=0"] acceptAnything
  return eOK

deviceSetFileSize _ _ = return eINVAL

deviceCreateDevice :: FilePath -> EntryType -> DeviceID -> DeviceCall Errno
deviceCreateDevice path RegularFile _ = do
  deviceCall ["shell", "touch", path] emptyResponse
  return eOK

deviceCreateDevice _ _ _= simpleError eINVAL

deviceCreateDirectory :: FilePath -> DeviceCall Errno
deviceCreateDirectory path = do
  deviceCall ["shell", "mkdir", path] emptyResponse
  return eOK


deviceDeleteFile :: FilePath -> DeviceCall Errno
deviceDeleteFile path = do
  deviceCall ["shell", "rm", "-f", path] emptyResponse
  return eOK

deviceDeleteDir :: FilePath -> DeviceCall Errno
deviceDeleteDir path = do
  deviceCall ["shell", "rm", "-fd", path] emptyResponse
  return eOK

isFileMode :: FileMode -> FileMode -> Bool
isFileMode whatToCheck modeOfQuestion = (whatToCheck `intersectFileModes` fileTypeModes) == modeOfQuestion


statFromRemoteFsEntry :: FuseContext -> RemoteFsEntry -> (FilePath, FileStat)
statFromRemoteFsEntry ctx (RemoteFsEntry fileMode size name) =
    (name, FileStat { statEntryType = fromMaybe RegularFile $ snd <$> (find ((fileMode `isFileMode`) . fst)
                                                                   [(blockSpecialMode, BlockSpecial),
                                                                    (characterSpecialMode, CharacterSpecial),
                                                                    (namedPipeMode, NamedPipe),
                                                                    (directoryMode, Directory),
                                                                    (symbolicLinkMode, SymbolicLink),
                                                                    (socketMode, Socket)])
                    , statFileMode = fileMode
                    , statLinkCount = 2
                    , statFileOwner = fuseCtxUserID ctx
                    , statFileGroup = fuseCtxGroupID ctx
                    , statSpecialDeviceID = 0
                    , statFileSize = fromIntegral size
                    , statBlocks = 1
                    , statAccessTime = 0
                    , statModificationTime = 0
                    , statStatusChangeTime = 0})


