{-# LANGUAGE StrictData  #-}
module MdGraph.Parse
    ( ParseResult(..)
    , Parses(..)
    , parseDocumentIO
    ) where

import           MdGraph.Node
import           MdGraph.Parse.Pandoc

import           Aux.Either                     ( mapLeft )
import           Control.Concurrent.Async       ( mapConcurrently )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Data.Either                    ( partitionEithers
                                                , rights
                                                )
import           Data.HashMap.Internal.Strict   ( toList )
import qualified Data.HashSet                  as S
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import           Data.Text                     as T
import           Data.Text.IO                  as T
                                                ( readFile )
import           MdGraph.App                    ( App )
import           MdGraph.App.LogLevel           ( LogLevel(Debug) )
import           MdGraph.App.Logger             ( logDebug )
import           MdGraph.Config                 ( Config
                                                    ( defaultExtension
                                                    , libraryPath
                                                    )
                                                , HasConfig(getConfig)
                                                )
import           MdGraph.File.Internal          ( AbsolutePath(AbsolutePath)
                                                , fixLink
                                                )
import           Prelude                       as P
import           System.Directory               ( doesFileExist
                                                , getModificationTime
                                                )
import           System.FilePath                ( (</>) )
import           Text.Pandoc                    ( PandocError
                                                , PandocMonad(fileExists)
                                                )

data ParseError = FileNotFound | PandocFail PandocError
    deriving Show

data ParseResult = ParseResult
    { file  :: AbsolutePath
    , links :: [Link]
    , tags  :: [Tag]
    }
    deriving Show

parseDocumentIO :: AbsolutePath -> IO (Either ParseError ParseResult)
parseDocumentIO absolutePath@(AbsolutePath filePath) = do
    exists <- doesFileExist filePath
    if not exists
        then return . Left $ FileNotFound
        else do
            fileContent <- T.readFile filePath
            return . mapLeft PandocFail $ do
                PandocResult { tags, links } <- sieveLinks fileContent
                return $ ParseResult absolutePath
                                     (S.toList links)
                                     (S.toList tags)

class Parses m where
  parseDocument :: AbsolutePath -> m (Either ParseError ParseResult)
  parseDocuments :: [AbsolutePath] -> m [ParseResult]

instance Parses App where
    parseDocument file = do
        liftIO $ parseDocumentIO file
    parseDocuments files = do
        parseResults <- liftIO $ mapConcurrently parseDocumentIO files
        return $ rights parseResults
