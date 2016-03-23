
import Fs 
import Test.HUnit
import Data.Maybe (fromJust)

data AdbFakeCall = CallAdb ([String] -> String)
                 | RestartAdb (() -> Bool)


assertPathQualification path expected = TestCase $ assertEqual message expected actual
    where message = (path ++ " qualified as " ++ (show expected))
          actual = fromJust $ qualifyPath path
    

main = runTestTT $ 
       TestList [ assertPathQualification "/" FsRoot
                , assertPathQualification "/a" $ Device "a"]