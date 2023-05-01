module Aux.Common where
import           Data.Text                      ( Text )

explain :: b -> Maybe a -> Either b a
explain left mb = maybe (Left left) Right mb

forEither :: Either a b -> (a -> c) -> (b -> c) -> c
forEither eith whenLeft whenRight = either whenLeft whenRight eith

batch :: Int -> [a] -> [[a]]
batch batchSize []   = []
batch batchSize list = headBatch : batch batchSize remainder
  where (headBatch, remainder) = splitAt batchSize list
