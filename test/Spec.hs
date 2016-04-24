module Spec (spec) where

spec :: Spec
spec = do
    describe "My feature" $ do
        it "works" $ do
            property $ \i -> (i - 5) + 5 === i
