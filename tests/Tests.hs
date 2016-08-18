
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import Fs 
import Test.HUnit (Test(..)
                  , runTestTT
                  , assertEqual
                  , assertFailure
                  , assertBool)

    
import Data.Maybe (fromJust)
import Control.Monad.Reader

import System.Posix.Types
import System.Posix.Files

import Utils hiding (blockify)
import qualified Utils as U

import qualified Parsers as P

import Classes
import Types

blockify :: Int -> Int -> Int -> Block
blockify = U.blockify

userId :: UserID
userId = fromIntegral (2 :: Integer)

groupId :: GroupID
groupId = fromIntegral (4 :: Integer)

assertPathQualification path expected = TestCase $ assertEqual message expected actual
    where message = (path ++ " qualified as " ++ (show expected))
          actual = fromJust $ qualifyPath path

instance GetFuseContext (Reader (UserID, GroupID)) where
    fuseUID = fst <$> ask
    fuseGID = snd <$> ask

testEmptyDir 
    = TestCase $ assertEqual "emptyDirContents" [".", ".."] $ map fst stats
      where stats = runReader emptyDirContents (userId, groupId)

testParseFileR :: (Show a, Eq a) => String -> P.Parser a -> a -> Test
testParseFileR fileName parser expected = 
    TestCase $ do 
      fileContents <- readFile fileName

      assertEqual (show expected ++ " from file " ++ fileName) (Right expected) (P.parse parser fileContents)

testParseFileS :: String -> P.Parser a -> Test
testParseFileS fileName parser = 
    TestCase $ do 
      fileContents <- readFile fileName
      
      let assert (Right _) = return ()
          assert (Left err) = assertFailure $ "parsing file " ++ fileName ++ ":\n" ++ show err
                              
      assert $ P.parse parser fileContents
      
main = runTestTT $ 
       TestList [ TestLabel "path qualification" $ 
                            TestList [assertPathQualification "/" FsRoot
                                     -- this fails:
                                     -- , assertPathQualification "/a/" $ Device "a"
                                     , assertPathQualification "/a" $ Device "a"
                                     , assertPathQualification "/a/fs" $ DeviceFs "a" 
                                     , assertPathQualification "/a/fs/" $ InDeviceFs "a" "/"
                                     , assertPathQualification "/a/fs/b" $ InDeviceFs "a" "/b"]

                , TestLabel "parsers"  
                                ( let assertError str = TestCase $ assertBool ("errorneously parsed " ++ str) $ isLeft $ parse str
                                      isLeft (Left _) = True
                                      isLeft (Right _) = False
                                      parse = P.parse P.parseFileModeRWXFormat

                                      assertParsed str flags = TestCase $ assertEqual ("parsing " ++ str) (Right (mconcat flags)) $ parse str
                                  in TestList [ assertError "adf"
                                              , assertError ""
                                              , assertParsed "d---------" [directoryMode]
                                              , assertParsed "l---------" [symbolicLinkMode]
                                              , assertParsed "lr---w---x" [symbolicLinkMode
                                                                          , ownerReadMode
                                                                          , groupWriteMode
                                                                          , otherExecuteMode]
                                              , testParseFileR "test-inputs/device-stat/genymotion-android-7.0-preview.txt"
                                                              P.remoteLsLine
                                                                   (RemoteFsEntry 
                                                                    (mconcat [ directoryMode
                                                                             , ownerReadMode
                                                                             , ownerExecuteMode
                                                                             , ownerWriteMode
                                                                             , groupReadMode
                                                                             , groupExecuteMode
                                                                             , otherReadMode
                                                                             , otherExecuteMode ])
                                                                    0
                                                                    "/")
                                              , testParseFileR "test-inputs/device-stat/genymotion-android-5.0.txt"
                                                              P.remoteLsLine
                                                                   (RemoteFsEntry 
                                                                    (mconcat [ directoryMode
                                                                             , ownerReadMode
                                                                             , ownerExecuteMode
                                                                             , ownerWriteMode
                                                                             , groupReadMode
                                                                             , groupExecuteMode
                                                                             , otherReadMode
                                                                             , otherExecuteMode ])
                                                                    0
                                                                    "")
                                              , testParseFileR "test-inputs/device-stat/genymotion-android-5.0-with-size.txt"
                                                              P.remoteLsLine
                                                                   (RemoteFsEntry 
                                                                    (mconcat [ ownerReadMode
                                                                             , ownerWriteMode
                                                                             , groupReadMode
                                                                             , groupWriteMode ])
                                                                    230000000
                                                                    "space-taker")
                                              , testParseFileS "test-inputs/ls-read-dir/genymotion-5.0.txt" 
                                                                P.rfseFromAdbLs
                                              , testParseFileS "test-inputs/ls-read-dir/genymotion-7.0.txt" 
                                                               P.rfseFromAdbLs
                                              , testParseFileS "test-inputs/device-stat/genymotion-5.0-line-with-symlink.txt"
                                                               P.remoteLsLine
                                              , testParseFileS "test-inputs/device-stat/genymotion-5.0-random-line-00.txt"
                                                               P.remoteLsLine
                                              ] )

                , TestLabel "blockfiy" $ TestList 
                            (let assert expected act = TestCase $ assertEqual "block equals" expected act
                             in [ assert (Block 0 100 1 0)  $ blockify 100 10 0
                                , assert (Block 0 100 1 1)  $ blockify 100 10 1
                                , assert (Block 0 100 2 95) $ blockify 100 10 95 ])

                                            
                , testEmptyDir ]