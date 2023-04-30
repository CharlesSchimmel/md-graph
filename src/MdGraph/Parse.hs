module MdGraph.Parse
    ( ParseResult(..)
    , Parses(..)
    ) where

import           MdGraph.Node
import           MdGraph.Parse.Pandoc

import           Control.Concurrent.Async       ( mapConcurrently )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Data.Either                    ( partitionEithers )
import           Data.HashMap.Internal.Strict   ( toList )
import qualified Data.HashSet                  as S
                                                ( toList )
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import           Data.Text                     as T
import           Data.Text.IO                  as T
                                                ( readFile )
import           MdGraph.App                    ( App )
import           MdGraph.App.LogLevel           ( LogLevel(Debug) )
import           MdGraph.Config                 ( Config(defaultExtension)
                                                , HasConfig(getConfig)
                                                )
import           MdGraph.File.Internal          ( fixLink )
import           Prelude                       as P
import           System.Directory               ( doesFileExist
                                                , getModificationTime
                                                )
import           System.FilePath                ( (</>) )
import           Text.Pandoc                    ( PandocMonad(fileExists) )

data ParseResult = ParseResult
    { file  :: FilePath
    , links :: [Link]
    , tags  :: [Tag]
    }
    deriving Show

-- TODO: Pull out absolute path
parseDocumentIO :: FilePath -> FilePath -> FilePath -> IO (Maybe ParseResult)
parseDocumentIO defExt libraryPath file = do
    exists <- doesFileExist absFile
    if not exists
        then return Nothing
        else do
            fileContent <- T.readFile absFile
            return $ do
                PandocResult { tags, links } <- sieveLinks fileContent
                return $ ParseResult file (S.toList links) (S.toList tags)
    where absFile = libraryPath </> file

class Parses m where
  parseDocuments :: [FilePath] -> m [ParseResult]

instance Parses App where
    parseDocuments files = do
        defExt  <- defaultExtension <$> getConfig
        libPath <- defaultExtension <$> getConfig
        liftIO
            $   catMaybes
            <$> mapConcurrently (parseDocumentIO defExt libPath) files
