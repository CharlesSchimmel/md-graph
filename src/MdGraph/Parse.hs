{-# LANGUAGE StrictData  #-}
module MdGraph.Parse
    ( ParseResult(..)
    , Parses(..)
    , parseDocumentIO
    ) where

import           MdGraph.Node
import           MdGraph.Parse.Pandoc

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
import           MdGraph.File.Internal          ( fixLink )
import           MdGraph.Util                   ( mapLeft
                                                , trace'
                                                , trace''
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
    { file  :: FilePath
    , links :: [Link]
    , tags  :: [Tag]
    }
    deriving Show

parseDocumentIO
    :: FilePath -> FilePath -> FilePath -> IO (Either ParseError ParseResult)
parseDocumentIO defExt libraryPath file = do
    exists <- doesFileExist absFile
    if not exists
        then return . Left $ FileNotFound
        else do
            fileContent <- T.readFile absFile
            return . mapLeft PandocFail $ do
                PandocResult { tags, links } <- sieveLinks fileContent
                return $ ParseResult file (S.toList links) (S.toList tags)
    where absFile = libraryPath </> file

class Parses m where
  parseDocuments :: [FilePath] -> m [ParseResult]

instance Parses App where
    parseDocuments files = do
        defExt       <- defaultExtension <$> getConfig
        libPath      <- libraryPath <$> getConfig
        parseResults <- liftIO
            $ mapConcurrently (parseDocumentIO defExt libPath) files
        return $ rights parseResults
