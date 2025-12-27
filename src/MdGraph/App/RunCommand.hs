{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module MdGraph.App.RunCommand where

import Aux.HashSet
import Control.Applicative (Alternative ((<|>)), Applicative (liftA2))
import Control.Exception (throwIO)
import Control.Monad (join)
import qualified Control.Monad as Monad
import Control.Monad.Except
  ( MonadError (throwError),
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.Foldable as F
import Data.HashSet as S
import Data.Hashable (Hashable)
import qualified Data.List as L
import qualified Data.List as List
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Database.Persist (Entity (entityVal))
import GHC.Generics (Generic)
import GHC.IO.Encoding (getForeignEncoding)
import MdGraph.App
  ( App,
    Env (config),
  )
import MdGraph.App.Command
import MdGraph.App.Logger
  ( Logs,
    logDebug,
    logInfo,
  )
import MdGraph.Config
  ( Config (..),
    HasConfig (getConfig),
  )
import MdGraph.File
  ( Files (maybeFile),
    trueAbsolutePath,
  )
import MdGraph.Persist.Class (Queries (..))
import MdGraph.Persist.Query as Q
import MdGraph.Persist.Schema
import qualified MdGraph.Persist.Schema as Edge
  ( Edge (..),
  )
import MdGraph.TagDirection
import MdGraph.Util (trace'')
import System.Directory (canonicalizePath)
import System.FilePath (makeRelative, (</>))

runCommand :: Command -> App [String]
runCommand Orphans = fmap documentPath <$> runOrphans
runCommand Unreachable = fmap documentPath <$> runUnreachable
runCommand Nonexes = fmap edgeHead <$> runNonexistent
runCommand (Subgraph options) = runSubgraph options
runCommand (Backlinks options) = runBacklinks options
runCommand Statics = throwError "NYI"
runCommand Populate = pure mempty

runOrphans :: (Monad m, Queries m, Logs m) => m [Document]
runOrphans = do
  orphanDocs <- getOrphans
  logDebug $ T.unwords ["Found", T.pack . show . length $ orphanDocs, "orphans"]
  return $ entityVal <$> orphanDocs

runUnreachable :: (Monad m, Queries m, Logs m) => m [Document]
runUnreachable = do
  orphanDocs <- getUnreachables
  logDebug $
    T.unwords ["Found", T.pack . show . length $ orphanDocs, "unreachable"]
  return $ entityVal <$> orphanDocs

runNonexistent :: (Monad m, Queries m, Logs m) => m [Edge]
runNonexistent = do
  nonexes <- getNonexistents
  logDebug $
    T.unwords ["Found", T.pack . show . length $ nonexes, "nonexistent"]
  return $ entityVal <$> nonexes

-- | Resolve a document if possible, otherwise return the edge
data SgResult = SgEdge {unSgEdge :: FilePath} | SgDocument {unSgDocument :: FilePath}
  deriving (Show, Eq, Generic)

instance Hashable SgResult

sgResultPath :: SgResult -> FilePath
sgResultPath (SgEdge path) = path
sgResultPath (SgDocument path) = path

isDocument :: SgResult -> Bool
isDocument (SgDocument _) = True
isDocument _ = False

runSubgraph ::
  forall m.
  (Monad m, Queries m, Logs m, Files m, HasConfig m) =>
  SubgraphOptions ->
  m [FilePath]
runSubgraph options@SubgraphOptions {sgTargets, sgMaxDepth, sgInclNonex, sgInclStatic} = do
  logInfo . T.unwords $ ["Finding subgraphs"]
  pathsSet <-
    F.foldrM
      (flip $ runSubgraphOnArg linkGetter sgMaxDepth)
      S.empty
      sgTargets
  let paths = S.toList pathsSet
  filteredResults <- processResults paths options

  -- If not inclNonex and not inclStatic, only return SgDocuments
  -- If inclStatic, return SgDocuments and SgEdges where the path exists (will need to absolutize that path)
  -- If inclNonex
  return $ Prelude.map sgResultPath filteredResults
  where
    linkGetter path = do
      queryResults <- getForwardLinks path
      let childPaths = Prelude.map (either (SgEdge . edgeHead . entityVal) (SgDocument . documentPath . entityVal)) queryResults
      return childPaths
    processResults ::
      (Monad m, Files m, HasConfig m) =>
      [SgResult] -> -- results
      SubgraphOptions ->
      m [SgResult]
    processResults results SubgraphOptions {sgInclNonex = True, sgInclStatic = True} = return results
    processResults results SubgraphOptions {sgInclNonex = False, sgInclStatic = False} = return $ List.filter isDocument results
    processResults results SubgraphOptions {sgInclNonex = False, sgInclStatic = True} = do
      Config {libraryPath} <- getConfig
      tryResolveEdgePaths <- Monad.forM results $ \case
        doc@(SgDocument docPath) -> return $ Just doc
        edge@(SgEdge edgePath) -> do
          plainPath <- maybeFile edgePath
          libraryEdgePath <- maybeFile $ libraryPath </> edgePath
          -- We're not actually trying to get the real path here, we just want to
          -- see if the file exists and then return the edgepath.
          return $ (edge <$ (plainPath <|> libraryEdgePath))
      return $ catMaybes tryResolveEdgePaths
    processResults results SubgraphOptions {sgInclNonex = True, sgInclStatic = False} = do
      Config {libraryPath} <- getConfig
      tryResolveEdgePaths <- Monad.forM results $ \case
        doc@(SgDocument docPath) -> return $ Just doc
        edge@(SgEdge edgePath) -> do
          plainPath <- maybeFile edgePath
          libraryEdgePath <- maybeFile $ libraryPath </> edgePath
          -- In this case, we _want_ the nonexistent edges, so we're flipping
          -- the maybe. (If the edge exists and it's `Just a` then flip it to
          -- Nothing).
          let edgeExists = plainPath <|> libraryEdgePath
          return $ maybe (Just edge) (const Nothing) edgeExists
      return $ catMaybes tryResolveEdgePaths

-- | Gets the children of the provided path
type LinkGetter m = FilePath -> m [SgResult]

runSubgraphOnArg ::
  (Monad m, HasConfig m, Files m) =>
  LinkGetter m ->
  Integer ->
  HashSet SgResult -> -- foundPaths
  SubgraphTarget ->
  m (HashSet SgResult)
runSubgraphOnArg linkGetter maxDepth foundPaths (FileTarget path) = do
  libPath <- libraryPath <$> getConfig
  targetAbsolutePath <- trueAbsolutePath path
  -- Making this an SgDocument feels a little dirty because that implies we know it exists...
  let relPath = SgDocument $ makeRelative libPath targetAbsolutePath

  -- TODO: fix infinite depth to be a real value instead of this hack
  runSubgraphPath' linkGetter maxDepth 0 foundPaths relPath
runSubgraphOnArg _ _ _ _ = pure S.empty -- TODO: support tag subgraphs

runSubgraphPath' ::
  (Monad m) =>
  LinkGetter m ->
  -- | MaxDepth
  Integer ->
  -- | Current depth
  Integer ->
  -- | All previously found paths
  S.HashSet SgResult ->
  -- | The target file to find the subgraph of
  SgResult ->
  m (S.HashSet SgResult)
runSubgraphPath' linkGetter maxDepth currentDepth foundPaths newPath = do
  let alreadyExists = S.member newPath foundPaths
      pastMaxDepth = currentDepth == maxDepth
  if alreadyExists || pastMaxDepth
    then return foundPaths
    else do
      let setWithCurrent = S.insert newPath foundPaths

      childPaths <- linkGetter $ sgResultPath newPath

      F.foldrM
        (flip $ runSubgraphPath' linkGetter maxDepth (currentDepth + 1))
        setWithCurrent
        childPaths

runBacklinks :: BacklinkOptions -> App [FilePath]
runBacklinks options@BacklinkOptions {blTargets, blMaxDepth} = do
  logInfo . T.unwords $ ["Finding backlinks"]
  paths <-
    F.foldrM
      (flip $ runSubgraphOnArg linkGetter blMaxDepth)
      S.empty
      blTargets
  return . List.map sgResultPath . S.toList $ paths
  where
    linkGetter path = do
      queryResults <- getBackwardLinks path
      let childPaths = Prelude.map (SgDocument . documentPath . entityVal) queryResults
      return childPaths
