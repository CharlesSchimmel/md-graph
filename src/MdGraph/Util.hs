module MdGraph.Util where


import           Debug.Trace

trace' x = trace (show x) x
trace'' note x = trace (note ++ " " ++ show x) x
