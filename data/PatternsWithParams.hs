module PatternsWithParams where

data Maybe a = Just a | None
data MyBool = True | False

unwrap :: Maybe a -> MyBool -> a
unwrap None _ = error "kaboom"
unwrap (Just val) True = val
-- Missing Just False