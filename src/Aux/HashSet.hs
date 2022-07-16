module Aux.HashSet where

import           Data.HashSet                  as S
import           Data.Hashable
import           Data.Maybe

catMaybesS :: (Eq a, Hashable a) => HashSet (Maybe a) -> HashSet a
catMaybesS = S.foldr fold S.empty
  where
    fold (Just a) acc = a `S.insert` acc
    fold Nothing  acc = acc

