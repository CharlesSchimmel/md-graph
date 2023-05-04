module Aux.Either where

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft _  (Right x) = Right x
mapLeft fn (Left  a) = Left $ fn a
