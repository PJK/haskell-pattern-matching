module GathererSpec (spec) where

import           Control.Monad            (forM_, unless)
import           Data.Aeson               (eitherDecode)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as LB
import           Data.Either              (isLeft, isRight)
import           Gatherer
import           Language.Haskell.Exts    (fromParseResult, parseFile)
import           System.Directory         (doesFileExist, listDirectory,
                                           withCurrentDirectory)
import           System.FilePath.Posix    (takeExtension, (</>))
import           Test.Hspec
import           TestUtils

spec :: Spec
spec = do
    blackBoxSpec

blackBoxSpec :: Spec
blackBoxSpec = describe "Black box tests" $ do
    blackBoxParseTests

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

