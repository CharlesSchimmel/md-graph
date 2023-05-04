{-# LANGUAGE ApplicativeDo #-}

module Aux.Tuple where

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd fn (a, b) = (a, fn b)

mapToSnd :: (a -> b) -> [a] -> [(a, b)]
mapToSnd fn = map (\a -> (a, fn a))

mapToSndM :: Monad m => (a -> m b) -> [a] -> m [(a, b)]
mapToSndM fn items = sequence $ sequence <$> mapToSnd fn items

mapToFst :: (a -> b) -> [a] -> [(b, a)]
mapToFst fn = map (\a -> (fn a, a))

sequenceTuple :: Applicative m => (a, m b) -> m (a, b)
sequenceTuple (a, b) = do
  b' <- b
  return (a, b')
