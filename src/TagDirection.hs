module TagDirection where

import           Node

data TagDirection = In | Out | Both
    deriving Show

tagSwap :: TagDirection -> (Node, Node) -> [(Node, Node)]
tagSwap In   tup           = [tup]
tagSwap Out  (source, tag) = [(tag, source)]
tagSwap Both tup           = tup : tagSwap Out tup

