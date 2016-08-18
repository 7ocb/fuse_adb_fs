{-# LANGUAGE FlexibleContexts #-}

module Parsers
    (remoteLsLine,
     Parser,
     parse,
     acceptAnything,
     noSuchFile,
     adbFsPath,
     rfseFromAdbLs,
     filePathFromRealpathResponse,
     singleFileStat,
     emptyResponse,
     parseFileModeRWXFormat)
where

import Types
import System.Posix.Types
import System.Posix.Files
import System.FilePath

import Control.Monad (forM, void)
import Data.Maybe (fromMaybe)

import qualified Text.Parsec as P
import Text.Parsec ( oneOf
                   , optional
                   , count
                   , anyChar
                   , digit
                   , manyTill
                   , space
                   , many1
                   , choice
                   , try
                   , char
                   , string
                   , many
                   , eof
                   , noneOf
                   , (<|>))
type Parser = P.Parsec String ()

parse :: Parser a -> String -> Either P.ParseError a
parse p s = P.parse p "" s

wordId :: Parser String
wordId = anyChar `manyTill` space

spaces :: Parser String
spaces = many $ char ' '

consumeDate :: Parser ()
consumeDate = void $ (num 4) >> char '-' >> (num 2) >> char '-' >> (num 2) >> char ' ' >> (num 2) >> char ':' >> (num 2)
    where num cnt = count cnt digit

name :: Bool -> Parser String
name True = anyChar `manyTill` (string " -> ") <* name False
name False = anyChar `manyTill` newline


remoteLsLine :: Parser RemoteFsEntry
remoteLsLine = try remoteLsLineMinimal
               <|> try remoteLsLineMinimalAndSize
               <|> remoteLsLineFull

remoteLsLineFull :: Parser RemoteFsEntry
remoteLsLineFull = do
  mode <- parseFileModeRWXFormat
  -- skip number
  spaces >> many1 digit
  -- skip group
  spaces >> wordId
  -- skip user
  spaces >> wordId

  spaces

  size <- read <$> many1 digit
  -- skip date
  spaces >> consumeDate

  spaces

  name <- name $ isSymlink mode

  return $ RemoteFsEntry mode size name

remoteLsLineMinimal :: Parser RemoteFsEntry
remoteLsLineMinimal = do
  mode <- parseFileModeRWXFormat
  -- skip group
  spaces >> wordId
  -- skip user
  spaces >> wordId
  -- skip date
  spaces >> consumeDate

  spaces

  name <- name $ isSymlink mode

  return $ RemoteFsEntry mode 0 name

remoteLsLineMinimalAndSize :: Parser RemoteFsEntry
remoteLsLineMinimalAndSize = do
  mode <- parseFileModeRWXFormat
  -- skip group
  spaces >> wordId
  -- skip user
  spaces >> wordId
  -- get size

  spaces

  size <- read <$> many1 digit
  -- skip date
  spaces >> consumeDate

  spaces

  name <- name $ isSymlink mode

  return $ RemoteFsEntry mode size name

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
newline = try (string "\r\n")
          <|> try (string "\n")
          <|> string "\r"

tillEndOfLine :: Parser String
tillEndOfLine = anyChar `manyTill` newline



permissionDeniedItem :: Parser RemoteFsEntry
permissionDeniedItem = do 
  anyChar `manyTill` char '\''
  name <- anyChar `manyTill` (try $ string "' failed: Permission denied")
  newline 

  return $ RemoteFsEntry nullFileMode 0 $ takeFileName name

rfseFromAdbLs :: Parser [RemoteFsEntry]
rfseFromAdbLs = do 
  optional $ try $ do
                  string "total "
                  many1 digit
                  newline 

  many $ ( try remoteLsLine 
           <|> permissionDeniedItem )

emptyResponse :: Parser ()
emptyResponse = eof

acceptAnything :: Parser ()
acceptAnything = return ()

noSuchFile :: Parser () 
noSuchFile = void $ anyChar `manyTill` (string ": No such file or directory")

adbFsPath :: Parser PathQualification
adbFsPath = do
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

filePathFromRealpathResponse :: Parser FilePath
filePathFromRealpathResponse = anyChar `manyTill` newline

singleFileStat :: Parser (Maybe RemoteFsEntry)
singleFileStat = choice [Just <$> try remoteLsLine
                        , Just <$> try permissionDeniedItem
                        , noSuchFile >> return Nothing]