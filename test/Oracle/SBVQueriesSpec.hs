module Oracle.SBVQueriesSpec (spec) where

import           Data.Either           (isLeft, isRight)
import           Data.SBV              hiding (forAll)
import qualified Data.Set              as S
import           DataDefs
import           Language.Haskell.Exts (fromParseResult, parseFile)
import           Oracle.SBVQueries
import           Test.Hspec
import           Test.QuickCheck
import           TestUtils
import           Types

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

spec :: Spec
spec = do
    describe "boolESat" $ do
        it "figures out that True is satisfiable" $ do
            shouldBeSatisfiable $ LitBool True

        it "figures out that False is unsatisfiable" $ do
            shouldBeUnsatisfiable $ LitBool False

        it "figures out that any single variable is satisfiable" $ do
            once $ property $ \s -> shouldBeSatisfiable $ BoolVar s

        it "figures out the correct answer to these 'not' queries" $ do
            shouldBeSatisfiable   $ BoolNot (LitBool False)
            shouldBeUnsatisfiable $ BoolNot (LitBool True)
            shouldBeSatisfiable   $ BoolNot (BoolVar "x")

        it "figures out that a boolean variable can't be both true and false" $ do
            shouldBeUnsatisfiable
                $ BoolOp BoolAnd (BoolOp BoolEQ (BoolVar "x") (LitBool True))
                                 (BoolOp BoolEQ (BoolVar "x") (LitBool False))

        it "figures out the correct answer to these 'or' queries" $ do
            shouldBeSatisfiable   $ BoolOp BoolOr (LitBool False) (LitBool True)
            shouldBeUnsatisfiable $ BoolOp BoolOr (LitBool False) (LitBool False)
            shouldBeSatisfiable   $ BoolOp BoolOr (LitBool False) (BoolVar "x")

        it "figures out the correct answer to these 'and' queries" $ do
            shouldBeSatisfiable   $ BoolOp BoolAnd (LitBool True) (LitBool True)
            shouldBeUnsatisfiable $ BoolOp BoolAnd (LitBool True) (LitBool False)
            shouldBeSatisfiable   $ BoolOp BoolAnd (LitBool True) (BoolVar "x")
            shouldBeUnsatisfiable $ BoolOp BoolAnd (BoolVar "x") (LitBool False)

        it "figures out the correct answer to these 'eq' queries" $ do
            shouldBeSatisfiable   $ BoolOp BoolEQ (LitBool True) (LitBool True)
            shouldBeSatisfiable   $ BoolOp BoolEQ (LitBool False) (LitBool False)
            shouldBeUnsatisfiable $ BoolOp BoolEQ (LitBool True) (LitBool False)
            shouldBeSatisfiable   $ BoolOp BoolEQ (LitBool True) (BoolVar "x")
            shouldBeSatisfiable   $ BoolOp BoolEQ (BoolVar "x") (LitBool False)

        it "figures out the correct answer to these 'neq' queries" $ do
            shouldBeSatisfiable   $ BoolOp BoolNEQ (LitBool False) (LitBool True)
            shouldBeSatisfiable   $ BoolOp BoolNEQ (LitBool True) (LitBool False)
            shouldBeUnsatisfiable $ BoolOp BoolNEQ (LitBool False) (LitBool False)
            shouldBeSatisfiable   $ BoolOp BoolNEQ (LitBool True) (BoolVar "x")
            shouldBeSatisfiable   $ BoolOp BoolNEQ (BoolVar "x") (LitBool False)

        it "figures out basic facts about literal integers" $ do
            once $
              forAll arbitrary $ \i1 ->
                forAll arbitrary $ \i2 -> do
                  IntBoolOp IntLT  (IntLit i1) (IntLit i2) `shouldResolveTo` (i1 <  i2)
                  IntBoolOp IntLE  (IntLit i1) (IntLit i2) `shouldResolveTo` (i1 <= i2)
                  IntBoolOp IntGT  (IntLit i1) (IntLit i2) `shouldResolveTo` (i1 >  i2)
                  IntBoolOp IntGE  (IntLit i1) (IntLit i2) `shouldResolveTo` (i1 >= i2)
                  IntBoolOp IntEQ  (IntLit i1) (IntLit i2) `shouldResolveTo` (i1 == i2)
                  IntBoolOp IntNEQ (IntLit i1) (IntLit i2) `shouldResolveTo` (i1 /= i2)

        it "figures out that basic facts about a variable and a literal are always satisfiable" $ do
            once $
              forAll arbitrary $ \il ->
                forAll arbitrary $ \iv -> do
                  shouldBeSatisfiable $ IntBoolOp IntLT  (IntLit il) (IntVar iv)
                  shouldBeSatisfiable $ IntBoolOp IntLE  (IntLit il) (IntVar iv)
                  shouldBeSatisfiable $ IntBoolOp IntGT  (IntLit il) (IntVar iv)
                  shouldBeSatisfiable $ IntBoolOp IntGE  (IntLit il) (IntVar iv)
                  shouldBeSatisfiable $ IntBoolOp IntEQ  (IntLit il) (IntVar iv)
                  shouldBeSatisfiable $ IntBoolOp IntNEQ (IntLit il) (IntVar iv)

