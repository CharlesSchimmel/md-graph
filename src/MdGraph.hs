{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module MdGraph
  ( mdGraph,
  )
where

import Aux.Map as M
import Aux.Tuple
  ( mapSnd,
    mapToSnd,
    mapToSndM,
  )
import Control.Concurrent.Async (mapConcurrently)
import qualified Control.Monad as Monad
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader
  ( MonadReader (ask),
    asks,
  )
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Either (partitionEithers, rights)
import qualified Data.Either as Either
import Data.Foldable as F
  ( mapM_,
  )
import qualified Data.Foldable as Foldable
import Data.HashSet as S
  ( HashSet,
    fromList,
    member,
    toList,
  )
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.Persist.Sqlite
  ( Entity (entityVal),
    runSqlite,
    wrapConnection,
  )
import Database.Sqlite (open)
import MdGraph.App
import MdGraph.App.Arguments
import MdGraph.App.Command (Command)
import MdGraph.App.Logger
import MdGraph.App.RunCommand (runCommand)
import MdGraph.Config
import MdGraph.File (Files (..), unrelativize)
import MdGraph.File.Types (AbsolutePath (..), File (..), RelativePath (..))
import MdGraph.Node (Link (..))
import MdGraph.Node as Node
import MdGraph.Parse
  ( ParseResult (..),
    Parses (..),
  )
import MdGraph.Persist.Class (PreparesDb (..))
import qualified MdGraph.Persist.Mapper as Mapper
import MdGraph.Persist.Schema
  ( Document (documentPath),
    Edge (..),
    Key (..),
    TempDocument (tempDocumentPath),
    migrateAll,
    migrateMdGraph,
  )
import qualified MdGraph.Persist.Schema as Schema
import Options.Applicative
import System.FilePath
  ( makeRelative,
    (-<.>),
    (<.>),
    (</>),
  )
import Prelude as P

mdGraph :: Arguments -> IO (Either T.Text [String])
mdGraph args@Arguments {argCommand} = do
  conf <- runExceptT $ argsToConfig args
  -- TODO: Better error handling here
  Monad.join <$> mapM withConf conf
  where
    withConf conf = do
      conn <- open $ dbConnString conf
      sqlBackend <- wrapConnection conn (\_ _ _ _ -> return ())
      let env = Env conf sqlBackend
      -- It's a little pointless to have this function and mdGraph function.
      -- TODO: Merge them
      runExceptT (runReaderT (runApp $ prepareDbAndRun argCommand) env)
    prepareDbAndRun :: Command -> App [String]
    prepareDbAndRun command = do
      prepareDatabase
      logDebug . T.pack $ show command
      runCommand command

prepareDatabase ::
  (Monad m, HasConfig m, PreparesDb m, Logs m, Files m, Parses m) => m ()
prepareDatabase = do
  Config {defaultExtension, libraryPath, dbConnString} <- getConfig
  logDebug $ T.unwords ["Using library:", T.pack libraryPath]
  logDebug $ T.unwords ["Preparing database:", dbConnString]
  migrate

  -- find documents
  logDebug "Finding documents"
  foundDocuments <- findDocuments
  let totalCt = P.length foundDocuments
  let relativeFileMap = M.fromList' relativePath foundDocuments

  -- load all found documents into temp
  logDebug "Populating TempDocuments"
  insertTempDocuments $ Mapper.fromFile <$> foundDocuments

  logDebug "Pruning deleted Documents"
  deletedCt <- pruneDeletedDocuments

  logDebug "Pruning unchanged TempDocuments"
  unchangedCt <- pruneUnchangedTempDocuments

  logDebug "Pruning modified Documents"
  modifiedCt <- pruneModifiedDocuments

  logDebug "Finding new and modified TempDocuments"
  newTempDocs <- getNewDocuments
  let docsToInsert = Mapper.fromTempDocument . entityVal <$> newTempDocs
      newCt = P.length docsToInsert - fromIntegral modifiedCt

  reportDocumentCount totalCt "total documents"
  reportDocumentCount unchangedCt "unchanged"
  reportDocumentCount deletedCt "deleted"
  reportDocumentCount modifiedCt "modified"
  reportDocumentCount newCt "new"

  logDebug "Inserting new and modified Documents"
  newDocs <- insertDocuments docsToInsert

  let docKeyMap = M.flop (RelativePath . documentPath) newDocs

  logDebug "Parsing new and modified Documents"
  logDebug . T.pack . show $ M.keys docKeyMap

  let filesAndDocumentToParse = M.elems $ M.unionZip relativeFileMap docKeyMap

  (newEdges, newTags) <- parseDocumentsAndOrganizeResults filesAndDocumentToParse

  logInfo
    . T.unwords
    $ ["Found", T.pack . show . P.length $ newEdges, "new edges"]

  logDebug . T.pack . show $ newEdges
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

parseDocumentsAndOrganizeResults ::
  (Monad m, HasConfig m, Logs m, Files m, Parses m) =>
  [(File, Key Document)] ->
  m ([Edge], [Schema.Tag])
parseDocumentsAndOrganizeResults filesAndDocumentToParse = do
  parseErrorOrContext <- Monad.forM filesAndDocumentToParse $ \(file, document) -> do
    parseErrorOrResult <- parseDocument . absolutePath $ file
    return $ do
      -- Either Monad
      ParseResult {tags, links} <- parseErrorOrResult
      Right $
        PostParseCtx
          { ppcDocument = document,
            ppcFile = file,
            ppcTag = tags,
            ppcLinks = links
          }

  let (parseErrors, postParseCtxs) = Either.partitionEithers parseErrorOrContext

  Monad.when (Foldable.length parseErrors > 0) $ do
    logError "Failed to parse some files" -- TODO add more detail
  let documentsAndAbsoluteLinks = postParseCtxs >>= unrollUnrelativizeLinks

  let knownFilePaths = HashSet.fromList $ map (\(File {absolutePath}, _) -> unAbsolutePath absolutePath) filesAndDocumentToParse
  documentAndRelativeLinksWithExtensions <- Monad.forM documentsAndAbsoluteLinks $ \(doc, link) -> do
    linkWithExtension <- addExtensionIfFileExists knownFilePaths link
    relativeLink <- mkLinksRelativeToLibrary linkWithExtension
    return (doc, relativeLink)

  let newEdges = uncurry Mapper.toEdge <$> documentAndRelativeLinksWithExtensions

  let newTags =
        postParseCtxs
          >>= ( \PostParseCtx {ppcTag, ppcDocument} ->
                  Mapper.toTag ppcDocument <$> ppcTag
              )
  return $ (newEdges, newTags)

unrollUnrelativizeLinks :: PostParseCtx -> [(Key Document, AbsoluteLink)]
unrollUnrelativizeLinks PostParseCtx {ppcFile, ppcDocument, ppcLinks} = do
  link <- ppcLinks
  let (File {absolutePath = sourcePath}) = ppcFile
  [(ppcDocument, unrelativizeLink sourcePath link)]

-- | Use a Link's source file path to unrelativize the Link path.
unrelativizeLink :: AbsolutePath -> Link -> AbsoluteLink
unrelativizeLink path link@(Link {linkPath}) = AbsoluteLink $ link {linkPath = unrelativizedPath}
  where
    unrelativizedPath = unAbsolutePath $ unrelativize path linkPath

-- | Links don't necessarily have or need a file extension. Check if a link's
-- path exists when we append the default extension. If it does, use that
-- instead.
addExtensionIfFileExists ::
  (Monad m, Files m, HasConfig m) =>
  HashSet FilePath ->
  AbsoluteLink ->
  m AbsoluteLink
addExtensionIfFileExists knownFiles (AbsoluteLink link@(Link {linkPath})) = do
  Config {defaultExtension} <- getConfig

  pathExistsUnmodified <- checkPathExists knownFiles linkPath

  let pathWithExtension = linkPath -<.> defaultExtension
  pathExistsWithExtension <- checkPathExists knownFiles pathWithExtension

  let pathExists = pathExistsUnmodified <|> pathExistsWithExtension
  let checkedLink =
        maybe link (\checkedPath -> link {linkPath = checkedPath}) pathExists
  return . AbsoluteLink $ checkedLink

checkPathExists ::
  (Monad m, Files m) =>
  HashSet FilePath ->
  FilePath ->
  m (Maybe FilePath)
checkPathExists knownPaths path = do
  let pathAlreadyDiscovered = if S.member path knownPaths then Just path else Nothing

  pathExistsOnFilesystem <- maybeFile path
  return $ pathAlreadyDiscovered <|> pathExistsOnFilesystem

mkLinksRelativeToLibrary :: (Monad m, HasConfig m) => AbsoluteLink -> m RelativeLink
mkLinksRelativeToLibrary (AbsoluteLink link@Link {linkPath, linkText}) = do
  Config {libraryPath} <- getConfig
  return . RelativeLink $
    Link
      { linkText = linkText,
        -- What if the linkPath isn't a descendent of the library? I don't
        -- think it matters, in that case it should be some other absolute
        -- path.
        linkPath = makeRelative libraryPath linkPath
      }

data PostParseCtx = PostParseCtx
  { ppcFile :: File,
    ppcDocument :: Key Document,
    ppcLinks :: [Link],
    ppcTag :: [Tag]
  }
  deriving (Show)
