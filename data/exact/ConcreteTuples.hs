module ConcreteTuples where

quadrupleAnd :: (Bool, Bool, Bool, Bool) -> Bool
quadrupleAnd (True, True, True, True) = True
quadrupleAnd (False, _, _, _) = False
quadrupleAnd (_, False, _, _) = False
quadrupleAnd (_, _, False, _) = False
-- T, T, T, F is missing