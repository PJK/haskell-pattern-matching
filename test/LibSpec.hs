module LibSpec (spec) where

import           Control.Monad            (forM_, unless)
import           Data.Aeson               (eitherDecode)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as LB
import           Lib
import           System.Directory         (doesFileExist, listDirectory,
                                           withCurrentDirectory)
import           System.FilePath.Posix    (takeExtension, (</>))
import           Test.Hspec

spec :: Spec
spec = do
    blackBoxSpec

sourceFiles :: FilePath -> IO [FilePath]
sourceFiles dir
    =   map (\f -> dir </> f)
    <$> filter (\f -> takeExtension f == ".hs")
    <$> listDirectory dir

resultFileFor :: FilePath -> FilePath
resultFileFor fp = fp ++ ".expected"

setupNewResults :: IO ()
setupNewResults = withCurrentDirectory "data" $ do
    sfs <- sourceFiles "."
    forM_ sfs $ \fp -> do
        let rfp = resultFileFor fp
        resultsExist <- doesFileExist rfp
        unless resultsExist $ do
            putStrLn $ "Setting up new expected results file for " ++ fp
            results <- doItAll fp
            let resultsBS = encodePretty results
            LB.writeFile rfp resultsBS

blackBoxSpec :: Spec
blackBoxSpec = do
    runIO setupNewResults
    describe "Black box tests" $ do
        sfs <- runIO $ sourceFiles "data"
        forM_ sfs $ \fp -> do
            let rfp = resultFileFor fp
            it fp $ do
                eC <- LB.readFile rfp
                actual <- doItAll fp
                eitherDecode eC `shouldBe` Right actual -- This implicitly checks that decoding succeeds.









