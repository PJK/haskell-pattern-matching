module LibSpec (spec) where

import           Control.Monad            (forM_, unless)
import           Data.Aeson               (eitherDecode)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as LB
import           Data.Either              (isLeft, isRight)
import           Language.Haskell.Exts    (fromParseResult, parseFile)
import           Lib
import           System.Directory         (doesFileExist, listDirectory,
                                           withCurrentDirectory)
import           System.FilePath.Posix    (takeExtension, (</>))
import           Test.Hspec

spec :: Spec
spec = do
    blackBoxSpec

blackBoxSpec :: Spec
blackBoxSpec = describe "Black box tests" $ do
    blackBoxExactTests
    blackBoxParseTests

resultFileFor :: FilePath -> FilePath
resultFileFor fp = fp ++ ".expected"

setupNewResults :: IO ()
setupNewResults = withCurrentDirectory "data/exact" $ return ()
    -- sfs <- sourceFiles "."
    -- forM_ sfs $ \fp -> do
    --     let rfp = resultFileFor fp
    --     resultsExist <- doesFileExist rfp
    --     unless resultsExist $ do
    --         putStrLn $ "Setting up new expected results file for " ++ fp
    --         results <- doItAll fp
    --         let resultsBS = encodePretty results
    --         LB.writeFile rfp resultsBS

blackBoxExactTests :: Spec
blackBoxExactTests = describe "Black box tests" $ return ()
    -- runIO setupNewResults
    -- sfs <- runIO $ sourceFiles "data/exact"
    -- forM_ sfs $ \fp -> do
    --     let rfp = resultFileFor fp
    --     it fp $ do
    --         eC <- LB.readFile rfp
    --         actual <- doItAll fp
    --         eitherDecode eC `shouldBe` Right actual -- This implicitly checks that decoding succeeds.

blackBoxParseTests :: Spec
blackBoxParseTests = do
    describe "Parse tests" $ do
        describe "Expected successful parses" $ do
            forSourcesIn "data/shouldParse" $ \fp -> do
                ast <- fromParseResult <$> parseFile fp
                getTypes ast `shouldSatisfy` isRight
                getFunctions ast `shouldSatisfy` isRight

        describe "Expected unsuccessful parses" $ do
            forSourcesIn "data/shouldNotParse" $ \fp -> do
                ast <- fromParseResult <$> parseFile fp
                let ress = (,) <$> getTypes ast <*> getFunctions ast
                ress `shouldSatisfy` isLeft

-- List the source files in a given directory
sourceFiles :: FilePath -> IO [FilePath]
sourceFiles dir
    =   map (\f -> dir </> f)
    <$> filter (\f -> takeExtension f == ".hs")
    <$> listDirectory dir


-- | Sets up a test case for every sourcefile in the given dir path
forSourcesIn :: FilePath -> (FilePath -> IO ()) -> Spec
forSourcesIn dir func = do
    sfs <- runIO $ sourceFiles dir
    forM_ sfs $ \fp -> it fp $ func fp



