module Aux.Common where

import Data.Text (Text)

explain :: b -> Maybe a -> Either b a
explain left mb = maybe (Left left) Right mb

-- | It's Just `either` with the Either as the first param
forEither :: Either a b -> (a -> c) -> (b -> c) -> c
forEither eith whenLeft whenRight = either whenLeft whenRight eith

batch :: Int -> [a] -> [[a]]
batch batchSize [] = []
batch batchSize list = headBatch : batch batchSize remainder
  where
    (headBatch, remainder) = splitAt batchSize list

unbool :: (a -> Bool) -> a -> Maybe a
unbool test a = if test a then Just a else Nothing
