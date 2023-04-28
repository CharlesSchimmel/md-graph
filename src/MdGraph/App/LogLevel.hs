module MdGraph.App.LogLevel where

data LogLevel = Debug | Info | Error | None
   deriving (Show, Enum, Ord, Eq)

