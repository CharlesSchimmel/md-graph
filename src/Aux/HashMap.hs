{-# LANGUAGE ScopedTypeVariables #-}

module Aux.HashMap where

import Aux.Tuple
import Data.HashMap.Strict as HM
import Data.Hashable (Hashable)

fromList' :: (Eq k, Hashable k) => (v -> k) -> [v] -> HM.HashMap k v
fromList' keySelector = HM.fromList . mapToFst keySelector

groupBy :: forall k v. (Eq k, Hashable k) => (v -> k) -> [v] -> HashMap k [v]
groupBy keySelector items =
  let withKeys = fmap (\item -> (keySelector item, items)) items
      folder :: HashMap k [v] -> v -> HashMap k [v]
      folder map item = insertWith (++) (keySelector item) [item] map
   in Prelude.foldl folder mempty items
