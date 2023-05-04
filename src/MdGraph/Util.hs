module MdGraph.Util where


import           Debug.Trace

trace' x = trace (show x) x
trace'' note x = trace (note ++ " " ++ show x) x

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft _  (Right x) = Right x
mapLeft fn (Left  a) = Left $ fn a
