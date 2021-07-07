module File where

import           Prelude                       as P
import           System.FilePath

doubleDot :: FilePath
doubleDot = ".."

-- unsafe, need to switch to safe tail,
reRelativize :: FilePath -> FilePath -> FilePath
reRelativize source destination
    | not $ isRelative destination = destination
    | otherwise = joinDir $ parentedSourceDirs ++ unrelativeDest
  where
    sourceParts    = splitDirectories $ takeDirectory source
    destParts      = splitDirectories destination
    destRefsParent = P.any (/= doubleDot) destParts
    pops           = P.foldr
        (\cur acc -> if cur == doubleDot then P.tail . acc else acc)
        id
        destParts
    parentedSourceDirs = pops sourceParts
    unrelativeDest     = P.filter (/= doubleDot) destParts

joinDir []    = ""
joinDir paths = P.foldr1 (</>) paths
