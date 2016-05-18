module LibSpec (spec) where

import           Control.Monad            (forM_, unless)
import           Data.Aeson               (eitherDecode)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as LB
import           Lib
import           System.Directory         (doesFileExist, withCurrentDirectory)
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

setupNewResults :: IO ()
setupNewResults = withCurrentDirectory "data/exact" $ do
    sfs <- sourceFiles "."
    forM_ sfs $ \fp -> do
        let rfp = resultFileFor fp
        resultsExist <- doesFileExist rfp
        unless resultsExist $ do
            putStrLn $ "Setting up new expected results file for " ++ fp
            results <- processTarget fp
            let resultsBS = encodePretty results
            LB.writeFile rfp resultsBS

blackBoxExactTests :: Spec
blackBoxExactTests = describe "Black box tests" $ do
    runIO setupNewResults
    sfs <- runIO $ sourceFiles "data/exact"
    forM_ sfs $ \fp -> do
        let rfp = resultFileFor fp
        it fp $ do
            eC <- LB.readFile rfp
            actual <- processTarget fp
            eitherDecode eC `shouldBe` Right actual -- This implicitly checks that decoding succeeds.
