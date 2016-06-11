module ClauseProcessingSpec (spec) where

import           ClauseProcessing
import           Control.Monad.Except (runExceptT)
import           Control.Monad.Reader (runReader)
import           Control.Monad.State  (evalStateT)
import qualified Data.Set             as S
import           DataDefs
import           Test.Hspec
import           Types

runAnalyzer :: AnalyzerContext -> AnalyzerState -> Analyzer a -> Either AnalyzerError a
runAnalyzer c s func = flip runReader c $ flip evalStateT s $ runExceptT $ func


spec :: Spec
spec = do
    describe "freshVar" $ do
        it "should return variable pattern ~a when asked for the first fresh variable" $ do
            runAnalyzer (S.fromList []) (AnalyzerState 0) freshVar
                `shouldBe` Right (VariablePattern $ "~a")


