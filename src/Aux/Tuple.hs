module Aux.Tuple where

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd fn (a, b) = (a, fn b)

mapToSnd :: (a -> b) -> [a] -> [(a, b)]
mapToSnd fn = map (\a -> (a, fn a))

mapToFst :: (a -> b) -> [a] -> [(b, a)]
mapToFst fn = map (\a -> (fn a, a))
