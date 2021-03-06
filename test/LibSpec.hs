module LibSpec (spec) where

import           Control.Monad        (forM_)
import           Control.Monad.Reader (runReaderT)
import           Data.Aeson           (eitherDecode)
import qualified Data.ByteString.Lazy as LB
import           Lib
import           OptParse
import           System.Directory     (doesFileExist)
import           Test.Hspec
import           TestUtils

spec :: Spec
spec = do
    blackBoxSpec

blackBoxSpec :: Spec
blackBoxSpec = describe "Black box tests" $ do
    blackBoxExactTests

resultFileFor :: FilePath -> FilePath
resultFileFor fp = fp ++ ".expected"

blackBoxExactTests :: Spec
blackBoxExactTests = describe "Black box tests" $ do
    sfs <- runIO $ sourceFiles "data/exact"
    forM_ sfs $ \fp -> do
        let rfp = resultFileFor fp
        rfpexists <- runIO $ doesFileExist rfp
        it fp $ do
            if rfpexists
            then do
                eC <- LB.readFile rfp
                actual <- flip runReaderT defaultSettings $ processTarget fp
                Right actual `shouldBe` eitherDecode eC -- This implicitly checks that decoding succeeds.
            else
                pendingWith $ "results for " ++ fp ++ " are missing,\nCheck the result with 'cat " ++ fp ++ " && patterns " ++ fp ++ "'\nand generate the results with 'patterns dump-results " ++ fp ++ " > " ++ rfp ++ "'"
