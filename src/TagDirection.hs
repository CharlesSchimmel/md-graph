module TagDirection where

import           Node

data TagDirection = In | Out | Both
    deriving Show

tagSwap :: TagDirection -> (Node, Node) -> [(Node, Node)]
tagSwap Out  (    source, t@(Tag _)) = [(t, source)]
tagSwap Both tup@(source, t@(Tag _)) = tup : tagSwap Out tup
tagSwap _    tup                     = [tup]
