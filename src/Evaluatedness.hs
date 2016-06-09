module Evaluatedness where

import           ClauseProcessing
import qualified Data.Foldable    as DFo
import qualified Data.Map         as Map
import           DataDefs
import           Debug.Trace
import qualified Text.Show.Pretty as Pr
import           Types

for :: [a] -> (a -> b) -> [b]
for = flip map

produceEvaluatednesses :: FunctionTarget -> FunctionResult -> SolvedFunctionResult -> Evaluatedness
produceEvaluatednesses ft@(FunctionTarget (Function name _ _)) (FunctionResult et) (SolvedFunctionResult sccs)
    =
    --   trace (Pr.ppShow ft)
    -- $ trace (Pr.ppShow $ map capD et)
    -- $ trace (Pr.ppShow $ map (map svav . scapD) sccs)
    -- $
    --   (\e -> trace (Pr.ppShow e) e) $
      Evaluatedness name
    $ concat
        $ for et $ \cc ->
            for (capD cc) $ \(CVAV {valueAbstraction = va, gamma = g, delta = d}) ->
                ( va
                , for va $ evalness $ bottomAssertedVariables d
                )

evalness :: [Name] -> Pattern -> Pattern
evalness ns = go
  where
    go :: Pattern -> Pattern
    go (VariablePattern n)
        | n `elem` ns = VariablePattern n -- TODO replace this with all the constructors.
        | otherwise = WildcardPattern -- If there is no 'IsBottom' constraint for this var, it won't be evaluated here.
    go p@(LiteralPattern _ _) = p
    go (ConstructorPattern n ps) = ConstructorPattern n $ map go ps
    go (TuplePattern ps) = TuplePattern $ map go ps
    go EmptyListPattern = EmptyListPattern
    go (InfixConstructorPattern p1 n p2) = InfixConstructorPattern (go p1) n (go p2)
    go WildcardPattern = WildcardPattern
    go IntVariablePattern = IntVariablePattern
    go (GuardPattern p n) = GuardPattern (go p) n

bottomAssertedVariables :: ConstraintSet -> [Name]
bottomAssertedVariables = concatMap go . termConstraints
  where
    go (IsBottom n) = [n]
    go _ = []

