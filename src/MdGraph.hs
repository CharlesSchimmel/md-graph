{-# LANGUAGE ScopedTypeVariables #-}
module MdGraph where

import           Aux.Map                       as M
import           MdGraph.App
import           MdGraph.App.Arguments
import           MdGraph.App.Logger
import           MdGraph.Config
import           MdGraph.File                   ( Files(..) )
import           MdGraph.File.Internal          ( AbsolutePath(..)
                                                , File(..)
                                                , RelativePath(..)
                                                , fixLink_
                                                )
import           MdGraph.Parse                  ( ParseResult(..)
                                                , Parses(..)
                                                )
import           MdGraph.Persist.Mapper        as Mapper
import           MdGraph.Persist.Schema         ( Document(documentPath)
                                                , Edge(..)
                                                , Tag(..)
                                                , TempDocument(tempDocumentPath)
                                                , migrateAll
                                                , migrateMdGraph
                                                )

import           Control.Concurrent.Async       ( mapConcurrently )
import           Control.Monad                  ( forM )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Identity         ( Identity(..) )
import           Control.Monad.Reader           ( MonadReader(ask)
                                                , asks
                                                )
import           Data.Foldable                 as F
                                                ( mapM_ )
import           Data.HashSet                  as S
                                                ( HashSet
                                                , fromList
                                                , member
                                                , toList
                                                )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( catMaybes )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Database.Persist.Sqlite        ( Entity(entityVal)
                                                , runSqlite
                                                )
import           Debug.Trace                    ( trace )
import           MdGraph.App.Command            ( Command )
import           MdGraph.App.RunCommand         ( runCommand )
import           MdGraph.Node                   ( Link(..) )
import           MdGraph.Persist.Class          ( PreparesDb(..) )
import           MdGraph.Util                   ( trace'' )
import           Options.Applicative
import           Prelude
import           Prelude                       as P
                                                ( foldr
                                                , length
                                                , map
                                                , mapM
                                                , print
                                                , putStrLn
                                                )
import           System.FilePath                ( (<.>)
                                                , (</>)
                                                )

mdGraph :: Command -> App ()
mdGraph command = do
    prepareDatabase
    -- (flip $ maybe (pure ())) maybeCommand $ \command -> do
    logDebug . T.pack $ show command
    docPaths <- runCommand command
    liftIO $ F.mapM_ putStrLn docPaths

-- prepareDatabaseFile :: App ()
-- prepareDatabaseFile = do


prepareDatabase
    :: (Monad m, HasConfig m, PreparesDb m, Logs m, Files m, Parses m) => m ()
prepareDatabase = do
    Config { defaultExtension } <- getConfig
    logDebug "Preparing database"
    migrate

    -- find documents
    logDebug "Finding documents"
    docs <- findDocuments
    let totalCt        = P.length docs
        relativeDocMap = M.fromList' relativePath docs
        absoluteDocMap = M.fromList' absolutePath docs

    -- load all found documents into temp
    logDebug "Populating TempDocuments"
    insertTempDocuments $ Mapper.fromFile <$> docs

    logDebug "Pruning deleted Documents"
    deletedCt <- pruneDeletedDocuments

    logDebug "Pruning unchanged TempDocuments"
    unchangedCt <- pruneUnchangedTempDocuments

    logDebug "Pruning modified Documents"
    modifiedCt <- pruneModifiedDocuments

    logDebug "Finding new and modified TempDocuments"
    newTempDocs <- getNewDocuments
    let docsToInsert = fromTempDocument . entityVal <$> newTempDocs
        newCt        = P.length docsToInsert - fromIntegral modifiedCt

    reportDocumentCount totalCt     "total documents"
    reportDocumentCount unchangedCt "unchanged"
    reportDocumentCount deletedCt   "deleted"
    reportDocumentCount modifiedCt  "modified"
    reportDocumentCount newCt       "new"

    logDebug "Inserting new and modified Documents"
    newDocs <- insertDocuments docsToInsert

    let docKeyMap       = M.flop (RelativePath . documentPath) newDocs
        docPathsToParse = M.keys docKeyMap
        filesToParse    = catMaybes $ (M.!?) relativeDocMap <$> docPathsToParse
        absPathsToParse = absolutePath <$> filesToParse

    logDebug "Parsing new and modified Documents"
    logDebug . T.pack . show $ docPathsToParse
    parseResults <- parseDocuments absPathsToParse

    -- Take the parseResults and try to rerelativize their links
    -- updatedResults <- updateLinks parseResults
    let knownPaths = S.fromList $ unAbsolutePath . absolutePath <$> docs
    -- The links have absolute paths to the real files (if they exist)
    let resultsWithAbsoluteLinks =
            rerelParseResult knownPaths defaultExtension <$> parseResults
-- The links have paths relative to the library
        resultsWithRelativeLinks =
            updateResultLinks absoluteDocMap <$> resultsWithAbsoluteLinks

    let resultMap = M.fromList' file resultsWithRelativeLinks
        pathToKeyResult =
            P.map snd . M.toList $ M.unionWith' docKeyMap relativeDocMap
        newTagsAndEdges :: [([Tag], [Edge])]
        newTagsAndEdges = P.map
            (\(docKey, result) ->
                (fromParseResult docKey result, fromParseResult docKey result)
            )
            pathToKeyResult
        (newTags, newEdges) = P.foldr
            (\(accTags, accEdges) (tags, edges) ->
                (accTags ++ tags, accEdges ++ edges)
            )
            ([], [])
            newTagsAndEdges

    logInfo
        . T.unwords
        $ ["Found", T.pack . show . P.length $ newEdges, "new edges"]

    logDebug "Inserting new edges"
    insertEdges newEdges

    logInfo
        . T.unwords
        $ ["Found", T.pack . show . P.length $ newTags, "new tags"]
    logDebug "Inserting new tags"
    insertTags newTags

    pure ()

reportDocumentCount num reason = do
    logInfo . T.unwords $ [T.pack . show $ num, reason]
    pure ()

-- | Update links to include the extension if they were defined without one,
-- and their link-path.md resolves to a real file
updateLinks
    :: forall m . (Monad m, Files m) => [ParseResult] -> m [ParseResult]
updateLinks results = do
    let tryResolveDoc result@ParseResult { file, links } = do
            resolvedLinks <- mapM (tryResolveLink file) links
            return result { links = resolvedLinks }
        tryResolveLink :: (Monad m, Files m) => FilePath -> Link -> m Link
        tryResolveLink sourceFile link@Link { linkPath } = do
            actualPath <- relativizeWithExtension sourceFile linkPath
            return $ link { linkPath = trace'' "actual path " actualPath }

    P.mapM tryResolveDoc results

-- Take the list of known files
-- Take the parsed links
-- Try rerelativising the parsed links, and if that new link is in known files, update it.
newUpdateLink :: S.HashSet FilePath -> FilePath -> FilePath -> Link -> Link
newUpdateLink knownPaths defaultExtension sourcePath link@Link { linkPath } =
    link { linkPath = newPath }
  where
    newPath = runIdentity $ fixLink_
        (\f -> Identity $ S.member f knownPaths)
        defaultExtension
        sourcePath
        linkPath

-- | Update Link paths to be relative to the source file
-- This could maybe attach the File not Just check existance
rerelParseResult
    :: S.HashSet FilePath -> FilePath -> ParseResult -> ParseResult
rerelParseResult knownPaths defaultExtension result@ParseResult { links, file }
    = result { links = newLinks }
  where
    newLinks =
        newUpdateLink knownPaths defaultExtension (unAbsolutePath file)
            <$> links

updateLink' :: M.Map AbsolutePath File -> Link -> Link
updateLink' map link@Link { linkPath } = maybe
    link
    (\file -> link { linkPath = unRelativePath $ relativePath file })
    found
    where found = map M.!? AbsolutePath linkPath

updateResultLinks :: M.Map AbsolutePath File -> ParseResult -> ParseResult
updateResultLinks map result@ParseResult { links } =
    result { links = updateLink' map <$> links }
