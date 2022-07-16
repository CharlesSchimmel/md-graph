{-# LANGUAGE TupleSections #-}

module MdGraph.Parse where

import           MdGraph.Node
import           MdGraph.Parse.Pandoc

import           Control.Concurrent.Async       ( mapConcurrently )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text.IO                  as T
                                                ( readFile )
import           Prelude                       as P
import           System.Directory               ( getModificationTime )

parseFile :: FilePath -> IO [Node]
parseFile file = do
    content <- sieveLinks <$> T.readFile file
    return $ fromMaybe [] content

parseToTuples :: [FilePath] -> IO [(FilePath, Node)]
parseToTuples files = P.concat <$> mapConcurrently parseToTuple files
  where
    parseToTuple f = do
        lastModified <- getModificationTime f
        parsedNodes  <- parseFile f
        return $ fmap (f, ) parsedNodes

parseNodes :: FilePath -> [FilePath] -> IO [(Node, Node)]
parseNodes defExt files = parseToTuples files >>= mapConcurrently fixNodes
  where
    fixNodes (source, node) = do
        fixed <- fixNode defExt source node
        return (Link source, fixed)


