{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module MdGraph
  ( mdGraph,
    rerelativizeLink,
  )
where

import Aux.Map as M
import Aux.Tuple
  ( mapSnd,
    mapToSnd,
    mapToSndM,
  )
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad
  ( forM,
    join,
  )
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
import Debug.Trace (trace)
import MdGraph.App
import MdGraph.App.Arguments
import MdGraph.App.Command (Command)
import MdGraph.App.Logger
import MdGraph.App.RunCommand (runCommand)
import MdGraph.Config
import MdGraph.File (Files (..))
import MdGraph.File.Internal
  ( AbsolutePath (..),
    File (..),
    RelativePath (..),
    smartRelativizePath,
  )
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
import MdGraph.Util (trace'')
import Options.Applicative
import System.FilePath
  ( makeRelative,
    (<.>),
    (</>),
  )
import Prelude
import Prelude as P
  ( foldr,
    length,
    map,
    mapM,
    print,
    putStrLn,
  )

mdGraph :: Arguments -> IO (Either T.Text [String])
mdGraph args@Arguments {argCommand} = do
  conf <- runExceptT $ argsToConfig args
  -- TODO: Better error handling here
  join <$> mapM withConf conf
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
      relativeFileMap = M.fromList' relativePath foundDocuments

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
      filesAndDocumentToParse =
        M.elems $ M.unionZip relativeFileMap docKeyMap

  logDebug "Parsing new and modified Documents"
  logDebug . T.pack . show $ M.keys docKeyMap

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
  let knownDocumentPaths = S.fromList $ unAbsolutePath . absolutePath <$> foundDocuments

  -- For all of the parse results, convert their links into absolute links
  ctxAndItsAbsoluteLinks <- Monad.forM postParseCtxs $ \ppc -> do
    absoluteLinks <- mkAbsoluteLinks knownDocumentPaths defaultExtension ppc
    return (ppc, absoluteLinks)

  -- Unroll the list of PostParseCtxs and their links, and flatMap it to pair
  -- every ppcDocument with each of its Links
  -- [(doc,[link])] -> [(doc,link)]
  let docKeyAndAbsoluteLinks =
        do
          (PostParseCtx {ppcDocument}, links) <- ctxAndItsAbsoluteLinks
          link <- links
          [(ppcDocument, link)]

  -- The links have paths relative to their file
  let docKeyAndRelativeLinks =
        fmap (mkRelativeLinks libraryPath) <$> docKeyAndAbsoluteLinks

  let newEdges = uncurry Mapper.toEdge <$> docKeyAndRelativeLinks

  logInfo
    . T.unwords
    $ ["Found", T.pack . show . P.length $ newEdges, "new edges"]

  logDebug . T.pack . show $ newEdges
  logDebug "Inserting new edges"
  insertEdges newEdges

  let newTags =
        postParseCtxs
          >>= ( \PostParseCtx {ppcTag, ppcDocument} ->
                  Mapper.toTag ppcDocument <$> ppcTag
              )

  logInfo
    . T.unwords
    $ ["Found", T.pack . show . P.length $ newTags, "new tags"]
  logDebug "Inserting new tags"
  insertTags newTags

  pure ()

reportDocumentCount num reason = do
  logInfo . T.unwords $ [T.pack . show $ num, reason]
  pure ()

-- | For a ParseResult, try to rerelativize its Links relative to the parsed
-- file's absolute path
-- | TODO: Should this be relativizing to the library root? Is it?
mkAbsoluteLinks ::
  (Monad m, Files m) =>
  S.HashSet FilePath ->
  FilePath ->
  PostParseCtx ->
  m [AbsoluteLink]
mkAbsoluteLinks knownPaths defaultExtension ctx@PostParseCtx {ppcLinks, ppcFile} = do
  let relativizer = rerelativizeLink knownPaths defaultExtension (absolutePath ppcFile)
  newLinks <- Monad.mapM relativizer ppcLinks
  return $ AbsoluteLink <$> newLinks

mkRelativeLinks :: FilePath -> AbsoluteLink -> RelativeLink
mkRelativeLinks libraryPath (AbsoluteLink link@Link {linkPath, linkText}) =
  RelativeLink $
    Link
      { linkText = linkText,
        linkPath = makeRelative libraryPath linkPath
      }

-- Take the list of known files
-- Take the parsed links
-- Try rerelativising the parsed links, and if that new link is in known files, update it.

-- | Using a set of known real files, check if the Link's path can be coerced
-- into matching one of those real file paths
rerelativizeLink ::
  forall m.
  (Monad m, Files m) =>
  -- | Known filepaths
  S.HashSet FilePath ->
  -- | Default extension to try
  FilePath ->
  -- | The source path to rerelativize against (if foo.md has a link to ../bar.md, foo.md is the source)
  AbsolutePath ->
  -- | The link te rerelativize
  Link ->
  m Link
rerelativizeLink knownPaths defaultExtension (AbsolutePath sourcePath) link@Link {linkPath} = do
  newPath <- smartRelativizePath linkTester defaultExtension sourcePath linkPath
  return $ link {linkPath = newPath}
  where
    linkTester :: (Monad m, Files m) => FilePath -> m Bool
    linkTester path =
      if S.member path knownPaths
        then return True
        else do
          i <- maybeFile path
          return $ Maybe.isJust i

data PostParseCtx = PostParseCtx
  { ppcFile :: File,
    ppcDocument :: Key Document,
    ppcLinks :: [Link],
    ppcTag :: [Tag]
  }
  deriving (Show)
