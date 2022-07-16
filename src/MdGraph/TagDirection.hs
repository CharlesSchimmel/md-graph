module MdGraph.TagDirection where

import           MdGraph.Node

data TagDirection = In | Out | Both
    deriving Show

tagSwap :: TagDirection -> (Node, Node) -> [(Node, Node)]
tagSwap Out  (    source, t@(Tag _)) = [(t, source)]
tagSwap Both tup@(source, t@(Tag _)) = tup : tagSwap Out tup
tagSwap _    tup                     = [tup]

adjustTagDir :: TagDirection -> [(Node, Node)] -> [(Node, Node)]
adjustTagDir tagDir links = links >>= tagSwap tagDir

