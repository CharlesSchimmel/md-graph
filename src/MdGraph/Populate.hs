module MdGraph.Populate where

import Aux.Map as Map
import Control.Applicative (Alternative (..), Applicative (..), (<$>))
import qualified Control.Monad as Monad
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Database.Persist.Sqlite
  ( Entity (entityVal),
  )
import MdGraph.App.Command
import MdGraph.App.Logger
import MdGraph.Config
import MdGraph.File
import MdGraph.File.Types
import MdGraph.Node as Node
import MdGraph.Parse
import MdGraph.Persist.Class (PreparesDb (..))
import qualified MdGraph.Persist.Mapper as Mapper
import MdGraph.Persist.Schema
import qualified MdGraph.Persist.Schema as Schema
import System.FilePath
  ( makeRelative,
    (-<.>),
    (<.>),
    (</>),
  )
import Prelude as P

populate ::
  (Monad m, HasConfig m, PreparesDb m, Logs m, Files m, Parses m) =>
  PopulateOptions ->
  m ()
populate PopulateAll = do
  Config {libraryPath} <- getConfig
  let libraryFilePath = unAbsolutePath libraryPath
  populate $ PopulateTargets [libraryFilePath]
populate (PopulateTargets targets) = do
  -- TODO? Validate that the targets are in the Library?
  absoluteTargetPaths <- Monad.mapM trueAbsolutePath targets

  -- find documents
  logDebug "Finding documents"
  foundDocuments <- findDocuments absoluteTargetPaths
  let totalCt = Foldable.length foundDocuments
  let relativeFileMap = Map.fromList' relativePath foundDocuments

  -- load all found documents into temp
  logDebug "Populating TempDocuments"
  insertTempDocuments $ Mapper.fromFile <$> foundDocuments

  -- XXX: How to handle this in both populate cases? We do want to prune deleted documents when we do a full library scan.
  -- It's easy in the targets case, we can check if any of the passed in targets doesn't exist.
  -- If they use the individual target option, maybe we should skip the unchanged/modified steps?
  logDebug "Pruning deleted Documents"
  deletedCt <- pruneDeletedDocuments

  logDebug "Pruning unchanged TempDocuments"
  unchangedCt <- pruneUnchangedTempDocuments

  logDebug "Pruning modified Documents"
  modifiedCt <- pruneModifiedDocuments

  logDebug "Finding new and modified TempDocuments"
  newTempDocs <- getNewDocuments
  let docsToInsert = Mapper.fromTempDocument . entityVal <$> newTempDocs
      newCt = Foldable.length docsToInsert - fromIntegral modifiedCt

  reportDocumentCount totalCt "total documents"
  reportDocumentCount unchangedCt "unchanged"
  reportDocumentCount deletedCt "deleted"
  reportDocumentCount modifiedCt "modified"
  reportDocumentCount newCt "new"

  logDebug "Inserting new and modified Documents"
  newDocs <- insertDocuments docsToInsert

  let docKeyMap = Map.flop (RelativePath . documentPath) newDocs

  logDebug "Parsing new and modified Documents"
  logDebug . Text.pack . show $ Map.keys docKeyMap

  let filesAndDocumentToParse = Map.elems $ Map.unionZip relativeFileMap docKeyMap

  (newEdges, newTags) <- parseDocumentsAndOrganizeResults filesAndDocumentToParse

  logInfo
    . Text.unwords
    $ ["Found", Text.pack . show . Foldable.length $ newEdges, "new edges"]

  logDebug . Text.pack . show $ newEdges
  logDebug "Inserting new edges"
  insertEdges newEdges

  logInfo
    . Text.unwords
    $ ["Found", Text.pack . show . Foldable.length $ newTags, "new tags"]
  logDebug "Inserting new tags"
  insertTags newTags

  return ()

reportDocumentCount num reason = do
  logInfo . Text.unwords $ [Text.pack . show $ num, reason]
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

  let knownFilePaths = HashSet.fromList $ fmap (\(File {absolutePath}, _) -> unAbsolutePath absolutePath) filesAndDocumentToParse
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
  let pathAlreadyDiscovered = if HashSet.member path knownPaths then Just path else Nothing

  pathExistsOnFilesystem <- maybeFile path
  return $ pathAlreadyDiscovered <|> pathExistsOnFilesystem

mkLinksRelativeToLibrary :: (Monad m, HasConfig m) => AbsoluteLink -> m RelativeLink
mkLinksRelativeToLibrary (AbsoluteLink link@Link {linkPath, linkText}) = do
  Config {libraryPath = AbsolutePath {unAbsolutePath = libraryPath}} <- getConfig
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
    ppcTag :: [Node.Tag]
  }
  deriving (Show)
