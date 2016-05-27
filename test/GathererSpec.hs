module GathererSpec (spec) where

import           Data.Either           (isLeft, isRight)
import           Gatherer
import           Language.Haskell.Exts (fromParseResult, parseFile)
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
            forSourcesIn "data/exact" $ \fp -> do
                ast <- fromParseResult <$> parseFile fp
                getTypes ast `shouldSatisfy` isRight
                getFunctions ast `shouldSatisfy` isRight

        describe "Expected unsuccessful parses" $ do
            forSourcesIn "data/shouldNotParse" $ \fp -> do
                ast <- fromParseResult <$> parseFile fp
                let ress = (,) <$> getTypes ast <*> getFunctions ast
                ress `shouldSatisfy` isLeft

