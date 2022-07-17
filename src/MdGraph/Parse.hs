module MdGraph.Parse
    ( parseDocument
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
import           MdGraph.File                   ( fixLink )
import           Prelude                       as P
import           System.Directory               ( doesFileExist
                                                , getModificationTime
                                                )
import           System.FilePath                ( (</>) )
import           Text.Pandoc                    ( PandocMonad(fileExists) )

data ParseResult = ParseResult
    { file  :: FilePath
    , links :: [FilePath]
    , tags  :: [Text]
    }
    deriving Show

-- TODO: mapConcurrently should be used by consumer of these functions

parseDocument :: FilePath -> FilePath -> FilePath -> IO (Maybe ParseResult)
parseDocument defExt libraryPath file = do
    exists <- doesFileExist absFile
    if not exists
        then return Nothing
        else do
            parsedNodes <- parseFile absFile
            let (links, tags) = partitionEithers . P.map sortNode $ parsedNodes
            return . Just $ ParseResult file links tags
  where
    absFile = libraryPath </> file
    sortNode (Link path) = Left path
    sortNode (Tag  text) = Right text


parseFile :: FilePath -> IO [Node]
parseFile file = do
    content <- sieveLinks <$> T.readFile file
    return $ fromMaybe [] content

