module Aux.Functor where

for :: (Functor f) => f a -> (a -> b) -> f b
for = flip fmap
