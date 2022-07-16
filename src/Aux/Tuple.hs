module Aux.Tuple where

toSnd :: (b -> c) -> (a, b) -> (a, c)
toSnd fn (a, b) = (a, fn b)

