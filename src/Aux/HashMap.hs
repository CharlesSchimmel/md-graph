module Aux.HashMap where

import           Aux.Tuple

import           Data.HashMap.Strict           as HM
import           Data.Hashable                  ( Hashable )

fromList' :: (Eq k, Hashable k) => (v -> k) -> [v] -> HM.HashMap k v
fromList' keySelector = HM.fromList . mapToFst keySelector
