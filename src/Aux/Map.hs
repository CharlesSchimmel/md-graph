{-# LANGUAGE ScopedTypeVariables #-}

module Aux.Map where

import Aux.Functor
import Aux.Tuple
  ( mapSnd,
    mapToFst,
  )
import Data.Map
import Data.Maybe (catMaybes)
import Data.Tuple (swap)

-- | Rebuild the Map against a new key l computed from the value, with the original key as a value. Keep in mind that the new key still needs to be distinct.
flop :: (Ord l) => (v -> l) -> Map k v -> Map l k
flop keySelector map_ = fromList $ swap . mapSnd keySelector <$> toList map_

rekey :: (Ord l) => (v -> l) -> Map k v -> Map l v
rekey keySelector map_ =
  let keyToNewKeyAndValue = map_ `for` \value -> (keySelector value, value)
      newKeyAndValues = elems keyToNewKeyAndValue
   in fromList newKeyAndValues

-- | Join two maps
unionZip :: (Ord k) => Map k a -> Map k b -> Map k (a, b)
unionZip mapA mapB =
  fromList . catMaybes $ flip Prelude.map commonKeys $ \key -> do
    valueOne <- mapA !? key
    valueTwo <- mapB !? key
    pure (key, (valueOne, valueTwo))
  where
    commonKeys = keys $ mapA `intersection` mapB

-- | Build a map from a list of values using a function to select the key
fromList' :: (Eq k, Ord k) => (v -> k) -> [v] -> Map k v
fromList' keySelector = fromList . mapToFst keySelector
