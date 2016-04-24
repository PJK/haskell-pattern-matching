module Lib
    (
      patterns
    )
    where

import           Language.Haskell.Exts

patterns :: IO ()
patterns = do
    ast <- fromParseResult <$> parseFile "data/redundant.hs"
    print ast
    putStrLn $ prettyPrint ast


-- Input:
--   - Context of datatypes (but not other functions)
--   - AST of function (but no need for RHS)
