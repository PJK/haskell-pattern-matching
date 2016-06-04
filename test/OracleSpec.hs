module OracleSpec (spec) where

import           Data.Either           (isLeft, isRight)
import qualified Data.Set              as S
import           DataDefs
import           Language.Haskell.Exts (fromParseResult, parseFile)
import           Oracle
import           Oracle.TestUtils
import           Test.Hspec
import           Test.QuickCheck
import           TestUtils
import           Types

spec :: Spec
spec = do
    describe "resolveTrivialTypeEqualities" $ do
        it "leaves empty lists alone" $ do
            resolveTrivialTypeEqualities [] `shouldBe` []

        it "clears any list of only trivial constraints" $ do
            pending
            -- property $ \vs -> --
            --     resolveTrivialTypeEqualities (map (\t -> (t, t)) vs) `shouldBe` []

    describe "resolveVaribleEqualities" $ do
        it "leaves empty lists alone" $ do
            resolveVariableEqualities [] `shouldBe` []

        it "removes any single variable equality because that's definitely satisfiable" $ do
            property $ \(v1, v2) -> resolveVariableEqualities [VarsEqual v1 v2] `shouldBe` []

        it "removes all constraints if they only consist of variable equalities" $ do
            property $ \vts -> -- variable tuples
                resolveVariableEqualities (map (uncurry VarsEqual) vts) `shouldBe` []

        it "correcly resolves this unit test" $ do
            resolveVariableEqualities
                [ IsBottom "x"
                , VarsEqual "x" "y"
                , VarEqualsBool "y" (BoolOp BoolOr (LitBool True) (BoolVar "y"))
                , VarsEqual "y" "z"
                , VarEqualsCons "z" "True" []
                , Uncheckable "teehee"
                ]
              `shouldBe`
                [ IsBottom "z"
                , VarEqualsBool "z" (BoolOp BoolOr (LitBool True) (BoolVar "z"))
                , VarEqualsCons "z" "True" []
                , Uncheckable "teehee"
                ]

    describe "resolveBottoms" $ do
        it "leaves empty lists alone" $ do
            resolveBottoms [] `shouldBe` []

        it "removes any single IsBottom constraint because that's definitely satisfiable" $ do
            property $ \v -> resolveBottoms [IsBottom v] `shouldBe` []


    describe "resolveSatBools" $ do
        it "says empty lists are satisfiable" $ do
            constraintsShouldBeSatisfiable []

        it "correctly resolves this unit test" $ do
            constraintsShouldBeUnsatisfiable
                    [ VarEqualsBool "x" (LitBool True)
                    , VarEqualsBool "x" (LitBool False)
                    ]

