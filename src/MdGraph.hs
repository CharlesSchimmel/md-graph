{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
                                                , smartRelativizePath
                                                )
import           MdGraph.Parse                  ( ParseResult(..)
                                                , Parses(..)
                                                )
import           MdGraph.Persist.Mapper        as Mapper
import           MdGraph.Persist.Schema         ( Document(documentPath)
                                                , Edge(..)
                                                , Key(..)
                                                , TempDocument(tempDocumentPath)
                                                , migrateAll
                                                , migrateMdGraph
                                                )

import           Aux.Tuple                      ( mapSnd
                                                , mapToSnd
                                                , mapToSndM
                                                )
import           Control.Concurrent.Async       ( mapConcurrently )
import           Control.Monad                  ( forM
                                                , join
                                                )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Identity         ( Identity(..) )
import           Control.Monad.Reader           ( MonadReader(ask)
                                                , asks
                                                )
import           Data.Either                    ( rights )
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
import           MdGraph.Node                  as Node
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
                                                , makeRelative
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
    Config { defaultExtension, libraryPath } <- getConfig
    logDebug "Preparing database"
    migrate

    -- find documents
    logDebug "Finding documents"
    docs <- findDocuments
    let totalCt         = P.length docs
        relativeFileMap = M.fromList' relativePath docs

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
        filesToParse    = catMaybes $ (M.!?) relativeFileMap <$> docPathsToParse
        filesAndDocumentToParse =
            catMaybes
                $   (\doc ->
                        (, doc)
                            <$>  relativeFileMap
                            M.!? (RelativePath . documentPath $ doc)
                    )
                <$> M.elems newDocs
        absPathsToParse = absolutePath <$> filesToParse

    logDebug "Parsing new and modified Documents"
    logDebug . T.pack . show $ docPathsToParse

    rawParseResults <- mapToSndM (parseDocument . absolutePath . fst)
                                 filesAndDocumentToParse
    let resequencedResults = P.map
            (\((file, document), eith) -> (file, document, ) <$> eith)
            rawParseResults
    let postParseCtxs =
            (\(file, document, ParseResult { tags, links }) -> PostParseCtx
                    { ppcDocument = document
                    , ppcFile     = file
                    , ppcTag      = tags
                    , ppcLinks    = links
                    }
                )
                <$> rights resequencedResults
    let fileInsideEither =
            (\(file, eith) -> (file, ) <$> eith) <$> rawParseResults

    -- -- TODO: Log out parse failures
    let parseResults   = rights fileInsideEither
    -- parseResults <- parseDocuments absPathsToParse

    -- Make links relative to file

    -- Take the parseResults and try to rerelativize their links
    -- updatedResults <- updateLinks parseResults
    let knownRealPaths = S.fromList $ absolutePath <$> docs

    -- The links have absolute paths to the real files (if the link resolves)
    let fileAndItsAbsoluteLinks =
            fmap (mkAbsoluteLinks knownRealPaths defaultExtension)
                <$> parseResults
    let fileAndLinks =
            fileAndItsAbsoluteLinks >>= (\(file, links) -> (file, ) <$> links)

    -- The links have paths relative to their file
    let relativeLinksAndFiles =
            fmap (mkRelativeLinks libraryPath) <$> fileAndLinks
    let relLinksAndDocKey =
            catMaybes
                $   (\(file, link) ->
                        (, link) <$> (docKeyMap M.!? relativePath file)
                    )
                <$> relativeLinksAndFiles

    let newEdges = uncurry Mapper.toEdge <$> relLinksAndDocKey

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

mkPostParseTag :: File -> Node.Tag -> PostParseTag
mkPostParseTag File { relativePath } tag@Node.Tag { tagLabel } =
    PostParseTag { taggedFile = relativePath, tagLabel = tagLabel }

-- | For a ParseResult, try to rerelativize its Links relative to the parsed
-- file's absolute path
mkAbsoluteLinks
    :: S.HashSet AbsolutePath -> FilePath -> ParseResult -> [AbsoluteLink]
mkAbsoluteLinks knownPaths defaultExtension result@ParseResult { links, file }
    = AbsoluteLink <$> newLinks
  where
    newLinks = rerelativizeLink knownPaths defaultExtension file <$> links

mkRelativeLinks :: FilePath -> AbsoluteLink -> RelativeLink
mkRelativeLinks libraryPath (AbsoluteLink link@Link { linkPath, linkText }) =
    RelativeLink $ Link { linkText = linkText
                        , linkPath = makeRelative libraryPath linkPath
                        }

-- Take the list of known files
-- Take the parsed links
-- Try rerelativising the parsed links, and if that new link is in known files, update it.
-- | Using a set of known real files, check if the Link's path can be coerced
-- into matching one of those real file paths
rerelativizeLink
    :: S.HashSet AbsolutePath -> FilePath -> AbsolutePath -> Link -> Link
rerelativizeLink knownPaths defaultExtension (AbsolutePath sourcePath) link@Link { linkPath }
    = link { linkPath = newPath }
  where
    newPath = runIdentity $ smartRelativizePath
        (\f -> Identity $ S.member f (unAbsolutePath <$> knownPaths))
        defaultExtension
        sourcePath
        linkPath

-- | Forach link in a ParseResult, Lookup the link's (Absolute) path in the map
-- to find its associated File, then update the Link's path with the File's
-- relative path
updateResultsWithActualRelativeLinks
    :: M.Map AbsolutePath File -> ParseResult -> ParseResult
updateResultsWithActualRelativeLinks map result@ParseResult { links } = result
    { links = getRelativePathForLink map <$> links
    }
  where
    -- | Lookup the link's (Absolute) path in the map, and if found, 
    getRelativePathForLink :: M.Map AbsolutePath File -> Link -> Link
    getRelativePathForLink map link@Link { linkPath } = maybe
        link
        (\file -> link { linkPath = unRelativePath $ relativePath file })
        found
        where found = map M.!? AbsolutePath linkPath


data PostParseTag = PostParseTag
    { taggedFile :: RelativePath
    , tagLabel   :: T.Text
    }

data PostParseCtx = PostParseCtx
    { ppcFile     :: File
    , ppcDocument :: Document
    , ppcLinks    :: [Link]
    , ppcTag      :: [Tag]
    }
    deriving Show
