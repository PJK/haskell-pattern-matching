module OracleSpec (spec) where

import           Data.Either           (isLeft, isRight)
import qualified Data.Set              as S
import           DataDefs
import           Language.Haskell.Exts (fromParseResult, parseFile)
import           Oracle
import           Test.Hspec
import           Test.QuickCheck
import           TestUtils
import           Types

spec :: Spec
spec = do
    describe "resorveVaribleEqualities" $ do
        it "leaves empty lists alone" $ do
            resolveVariableEqualities [] `shouldBe` []

        it "Removes any single variable equality because that's definitely satisfiable" $ do
            property $ \(v1, v2) -> resolveVariableEqualities [VarsEqual v1 v2] `shouldBe` []

        it "Removes all constraints if they only consist of variable equalities" $ do
            property $ \vts -> -- variable tuples
                resolveVariableEqualities (map (uncurry VarsEqual) vts) `shouldBe` []

    describe "resolveBottoms" $ do
        it "leaves empty lists alone" $ do
            resolveBottoms [] `shouldBe` []

        it "Removes any single IsBottom constraint because that's definitely satisfiable" $ do
            property $ \v -> resolveBottoms [IsBottom v] `shouldBe` []

        -- TODO more exhaustive tests here please
