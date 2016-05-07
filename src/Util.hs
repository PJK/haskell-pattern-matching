module Util where

import qualified Data.Map as Map

-- | Invert mapping. Used to construct inverse of getTypeConstructorsMap that will be used
-- | to annotate ConstructorPattern arguments to be compatible with PatternVector
invertMap :: (Ord b) => Map.Map a [b] -> Map.Map b a
invertMap m = Map.fromList $ concatMap (\(k, vs) -> [(v, k) | v <- vs]) (Map.toList m)

