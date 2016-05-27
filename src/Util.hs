{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Util where

import qualified Data.Map               as Map
import           OptParse.Types

import           Control.Monad          (when)
import           Control.Monad.IO.Class
import           Control.Monad.Reader   (MonadReader (..), asks)
import qualified Text.Show.Pretty       as Pr


-- | Invert mapping. Used to construct inverse of getTypeConstructorsMap that will be used
-- | to annotate ConstructorPattern arguments to be compatible with PatternVector
invertMap :: (Ord b) => Map.Map a [b] -> Map.Map b a
invertMap m = Map.fromList $ concatMap (\(k, vs) -> [(v, k) | v <- vs]) (Map.toList m)

debug :: (MonadIO m, MonadReader Settings m) => String -> m ()
debug s = do
    debug <- asks setsDebug
    when debug $ liftIO $ putStrLn s

debugShow :: (MonadIO m, MonadReader Settings m, Show s) => s -> m ()
debugShow = debug . Pr.ppShow
