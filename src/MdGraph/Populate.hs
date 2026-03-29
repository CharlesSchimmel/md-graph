{-# LANGUAGE OverloadedRecordDot #-}

module MdGraph.Populate (populate) where

import Aux.Common (for)
import qualified Aux.HashMap as HashMap
import Aux.Map as Map
import Control.Applicative (Alternative (..), Applicative (..), (<$>))
import Control.Monad
import qualified Control.Monad as Monad
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List (group)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time
import Database.Persist.Sqlite
  ( Entity (entityVal),
  )
import MdGraph.App.Arguments
import MdGraph.App.Logger
import MdGraph.Config
import MdGraph.File
import MdGraph.File.Types
import MdGraph.Node as Node
import MdGraph.Parse
import MdGraph.Parse.Types (inferDocumentFormat)
import MdGraph.Persist.Class (PreparesDb (..))
import qualified MdGraph.Persist.Mapper as Mapper
import MdGraph.Persist.Schema
import qualified MdGraph.Persist.Schema as Schema
import System.FilePath
  ( dropExtension,
    (-<.>),
    (<.>),
    (</>),
  )

-- Populating all files and populating some files need to handle deleting documents slightly differently, so we'll parameterize that operation. Everything else is the same.
type DeleteDocumentsFn m num = [FoundDocument] -> m num

_populate ::
  (Monad m, HasConfig m, PreparesDb m, Logs m, Files m, Parses m, Show num, Num num) =>
  [AbsolutePath] ->
  DeleteDocumentsFn m num ->
  m ()
_populate targets doPruneDeleted = do
  Config {libraryPath} <- getConfig
  -- find documents
  logDebug "Finding documents"
  rawFoundFiles <- findDocuments targets

  let foundDocuments = fmap (mkFoundDocument libraryPath) rawFoundFiles
  let totalCt = Foldable.length foundDocuments
  let relativeFileMap = Map.fromList' fdRelativePath foundDocuments
  logDebug . Text.pack . show $ relativeFileMap

  -- load all found documents into temp
  logDebug "Populating TempDocuments"
  let tempDocuments = for foundDocuments $
        \FoundDocument {fdModificationTime, fdRelativePath} ->
          TempDocument
            { tempDocumentPath = fdRelativePath,
              tempDocumentModifiedAt = fdModificationTime
            }
  insertTempDocuments tempDocuments

  logDebug "Pruning deleted Documents"
  deletedCt <- doPruneDeleted foundDocuments

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

  let docKeyMap = Map.flop documentPath newDocs

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

-- | Scan for Documents, parse them for Edges and Tags, and populate the database with changes.
-- Would it make sense to return the found files?
populate ::
  (Monad m, HasConfig m, PreparesDb m, Logs m, Files m, Parses m) =>
  ScanOptions ->
  m ()
populate ScanNone = return ()
populate ScanAll = do
  Config {libraryPath} <- getConfig
  _populate [libraryPath] (const pruneDeletedDocuments)
populate (ScanSome targets) = do
  Config {libraryPath} <- getConfig
  -- TODO? Validate that the targets are in the Library?
  absoluteTargetPaths <- Monad.mapM trueAbsolutePath targets
  let deleteDocumentsFn foundDocuments =
        do
          -- If any of the target files weren't found but were in the database, delete them.
          -- This assumes that the target's aren't directories.
          let foundPathsSet =
                HashSet.fromList $
                  for foundDocuments $
                    \FoundDocument {fdAbsolutePath = fileAbsPath} -> makeRelative libraryPath fileAbsPath
          let targetPathsSet =
                HashSet.fromList $
                  for absoluteTargetPaths $
                    \absTargetPath -> makeRelative libraryPath absTargetPath
          let unfoundDocuments = Foldable.toList $ targetPathsSet `HashSet.difference` foundPathsSet
          deleteDocuments unfoundDocuments

  _populate absoluteTargetPaths deleteDocumentsFn

reportDocumentCount :: (Monad m, Logs m, Show num, Num num) => num -> Text.Text -> m ()
reportDocumentCount num reason = do
  logInfo . Text.unwords $ [Text.pack . show $ num, reason]

parseDocumentsAndOrganizeResults ::
  (Monad m, HasConfig m, Logs m, Files m, Parses m) =>
  [(FoundDocument, Key Document)] ->
  m ([Edge], [Schema.Tag])
parseDocumentsAndOrganizeResults filesAndDocumentToParse = do
  parseErrorOrContext <- Monad.forM filesAndDocumentToParse $ \(file, document) -> do
    let inferredDocumentType = inferDocumentFormat file.fdAbsolutePath.unAbsolutePath
    parseErrorOrResult <- parseDocument . fdAbsolutePath $ file
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

  let knownFiles = HashSet.fromList $ fmap (\(doc, _) -> doc.fdAbsolutePath.unAbsolutePath) filesAndDocumentToParse
  documentAndRelativeLinksWithExtensions <- Monad.forM documentsAndAbsoluteLinks $ \(doc, link) -> do
    linkWithExtension <- addExtensionIfFileExists knownFiles link
    relativeLink <- mkLinksRelativeToLibrary linkWithExtension
    return (doc, relativeLink)

  let allEdges = uncurry Mapper.toEdge <$> documentAndRelativeLinksWithExtensions
  let groupedEdges = Map.groupBy (\i -> (i.edgeHead, i.edgeTail)) allEdges
  -- TODO: There's a database constraint for unique edges that we probably don't need.
  let dedupedEdges = head <$> Map.elems groupedEdges

  let newTags =
        postParseCtxs
          >>= ( \PostParseCtx {ppcTag, ppcDocument} ->
                  Mapper.toTag ppcDocument <$> ppcTag
              )
  return $ (dedupedEdges, newTags)

unrollUnrelativizeLinks :: PostParseCtx -> [(Key Document, AbsoluteLink)]
unrollUnrelativizeLinks PostParseCtx {ppcFile, ppcDocument, ppcLinks} = do
  link <- ppcLinks
  let (FoundDocument {fdAbsolutePath = sourcePath}) = ppcFile
  [(ppcDocument, unrelativizeLink sourcePath link)]

-- | Use a Link's source file path to unrelativize the Link path.
unrelativizeLink :: AbsolutePath -> Link -> AbsoluteLink
unrelativizeLink path link@(Link {linkPath}) = AbsoluteLink $ link {linkPath = unrelativizedPath}
  where
    unrelativizedPath = unAbsolutePath $ unrelativize path linkPath

-- | Links don't necessarily have or need a file extension. Check if a link's
-- path exists when we append the default extension. If it does, use that
-- instead.
-- TODO: or check all known paths without extension. Trim the extension off of all the paths we've found and compare them
addExtensionIfFileExists ::
  (Monad m, Files m, HasConfig m) =>
  HashSet FilePath ->
  AbsoluteLink ->
  m AbsoluteLink
addExtensionIfFileExists knownFiles (AbsoluteLink link@(Link {linkPath})) = do
  Config {defaultExtension} <- getConfig

  pathExistsUnmodified <- checkPathExists knownFiles linkPath
  -- pathExistsWithoutExtension <- checkPathExists knownFilesWithoutExtension (dropExtension linkPath)

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
  Config {libraryPath} <- getConfig
  let absLinkPath = AbsolutePath linkPath -- kinda gross, but we already ensured it's an AbsoluteLink
  return . RelativeLink $
    Link
      { linkText = linkText,
        -- What if the linkPath isn't a descendent of the library? I don't
        -- think it matters, in that case it should be some other absolute
        -- path.
        linkPath = makeRelative libraryPath absLinkPath
      }

data PostParseCtx = PostParseCtx
  { ppcFile :: FoundDocument,
    ppcDocument :: Key Document,
    ppcLinks :: [Link],
    ppcTag :: [Node.Tag]
  }
  deriving (Show)

data FoundDocument = FoundDocument {fdRelativePath :: FilePath, fdModificationTime :: UTCTime, fdAbsolutePath :: AbsolutePath}
  deriving (Show)

mkFoundDocument :: AbsolutePath -> File -> FoundDocument
mkFoundDocument libraryPath File {absolutePath, modificationTime} =
  FoundDocument
    { fdRelativePath = makeRelative libraryPath absolutePath,
      fdAbsolutePath = absolutePath,
      fdModificationTime = modificationTime
    }
