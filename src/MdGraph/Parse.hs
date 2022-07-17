module MdGraph.Parse
    ( parseDocuments
    , ParseResult(..)
    ) where

import           MdGraph.Node
import           MdGraph.Parse.Pandoc

import           Control.Concurrent.Async       ( mapConcurrently )
import           Data.Either                    ( partitionEithers )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                     as T
import           Data.Text.IO                  as T
                                                ( readFile )
import           MdGraph.File                   ( Document(..)
                                                , fixLink
                                                )
import           Prelude                       as P
import           System.Directory               ( getModificationTime )

data ParseResult = ParseResult
    { file  :: FilePath
    , links :: [FilePath]
    , tags  :: [Text]
    }

parseDocuments :: FilePath -> [FilePath] -> IO [ParseResult]
parseDocuments defExt files = parseDocuments' files
    >>= mapConcurrently fixLink'
  where
    fixLink' lx@ParseResult { file, links } = do
        fixed <- P.mapM (fixLink defExt file) links
        return $ lx { links = fixed }


parseDocuments' :: [FilePath] -> IO [ParseResult]
parseDocuments' = mapConcurrently $ \file -> do
    parsedNodes <- parseFile file
    let (links, tags) = partitionEithers . P.map sortNode $ parsedNodes
    return $ ParseResult file links tags
  where
    sortNode (Link path) = Left path
    sortNode (Tag  text) = Right text

parseFile :: FilePath -> IO [Node]
parseFile file = do
    content <- sieveLinks <$> T.readFile file
    return $ fromMaybe [] content

