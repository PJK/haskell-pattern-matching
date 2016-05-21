module ClauseProcessingSpec (spec) where

import           ClauseProcessing
import           Control.Monad.Except  (runExceptT)
import           Control.Monad.Reader  (runReader)
import           Control.Monad.State   (evalStateT)
import           Data.Either           (isLeft, isRight)
import qualified Data.Set              as S
import           DataDefs
import           Language.Haskell.Exts (fromParseResult, parseFile)
import           Test.Hspec
import           Test.QuickCheck
import           TestUtils
import           Types

runAnalyzer :: AnalyzerContext -> AnalyzerState -> Analyzer a -> Either AnalyzerError a
runAnalyzer c s func = flip runReader c $ flip evalStateT s $ runExceptT $ func


spec :: Spec
spec = do
    describe "freshVar" $ do
        it "should return variable pattern 0 when asked for the first fresh variable" $ do
            runAnalyzer (S.fromList []) (AnalyzerState 0) freshVar
                `shouldBe` Right (VariablePattern $ "fresh" ++ show 0)

        it "should return the contained integer fresh value" $ do
            property $ \i ->
                (runAnalyzer (S.fromList []) (AnalyzerState i) freshVar)
                    `shouldBe` Right (VariablePattern $ "fresh" ++ show i)


