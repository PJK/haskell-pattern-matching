module Oracle.TestUtils where

import           Data.SBV          hiding (forAll)
import           DataDefs
import           Oracle
import           Oracle.SBVQueries
import           Test.Hspec

constraintsShouldBeSatisfiable :: [Constraint] -> IO ()
constraintsShouldBeSatisfiable cs = do
    res <- resolveSatBools cs
    case res of
        SatResult (Satisfiable _ _) -> return ()
        r -> expectationFailure $ show cs ++ "have not been found to be satisfiable: " ++ show r

constraintsShouldBeUnsatisfiable :: [Constraint] -> IO ()
constraintsShouldBeUnsatisfiable cs = do
    res <- resolveSatBools cs
    case res of
        SatResult (Unsatisfiable _) -> return ()
        r -> expectationFailure $ show cs ++ "have not been found to be unsatisfiable: " ++ show r

shouldBeSatisfiable :: BoolE -> IO ()
shouldBeSatisfiable b = do
    res <- boolESatResult b
    case res of
        SatResult (Satisfiable _ _) -> return ()
        r -> expectationFailure $ show b ++ " has not been found to be satisfiable: " ++ show r

shouldBeUnsatisfiable :: BoolE -> IO ()
shouldBeUnsatisfiable b = do
    res <- boolESatResult b
    case res of
        SatResult (Unsatisfiable _) -> return ()
        r -> expectationFailure $ show b ++ " has not been found to be unsatisfiable:\n" ++ show r

shouldResolveTo :: BoolE -> Bool -> IO ()
shouldResolveTo be True = shouldBeSatisfiable be
shouldResolveTo be False = shouldBeUnsatisfiable be
