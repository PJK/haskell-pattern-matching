module Evaluatedness where

import           DataDefs
import           Debug.Trace
import qualified Text.Show.Pretty as Pr
import           Types

for :: [a] -> (a -> b) -> [b]
for = flip map

produceEvaluatednesses :: FunctionTarget -> FunctionResult -> SolvedFunctionResult -> Evaluatedness
produceEvaluatednesses ft@(FunctionTarget (Function name _ _)) (FunctionResult et) (SolvedFunctionResult sccs)
    = trace (Pr.ppShow ft)
    $ trace (Pr.ppShow $ map capD et)
    $ trace (Pr.ppShow $ map (map svav . scapD) sccs)
    $ Evaluatedness name
    $ concat
        $ for et $ \cc ->
            for (capD cc) $ \cvav ->
                let va = valueAbstraction cvav
                in
                ( va
                , for va $ evalness $ bottomAssertedVariables $ delta cvav
                )

evalness :: [Name] -> Pattern -> [Pattern]
evalness ns = go
  where
    go :: Pattern -> [Pattern]
    go (VariablePattern n)
        | n `elem` ns = [VariablePattern n] -- TODO replace this with all the constructors.
        | otherwise = [] -- If there is no 'IsBottom' constraint for this var, it won't be evaluated here.
    go p@(LiteralPattern _ _) = [p]
    go (ConstructorPattern n []) = [ConstructorPattern n []]
    go (ConstructorPattern n ps) = map (ConstructorPattern n . go) ps
    go (TuplePattern ps) = map (TuplePattern . go) ps
    go EmptyListPattern = [EmptyListPattern]
    go (InfixConstructorPattern p1 n p2) = [InfixConstructorPattern p n q | p <- go p1, q <- go p2]
    go WildcardPattern = [WildcardPattern]
    go IntVariablePattern = [IntVariablePattern]
    go (GuardPattern p n) = map (`GuardPattern` n) $ go p

bottomAssertedVariables :: ConstraintSet -> [Name]
bottomAssertedVariables = concatMap go . termConstraints
  where
    go (IsBottom n) = [n]
    go _ = []






