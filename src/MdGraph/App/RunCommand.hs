{-#  LANGUAGE RankNTypes  #-}
{-# LANGUAGE  NamedFieldPuns  #-}
module MdGraph.App.RunCommand where

import           Aux.HashSet
import           MdGraph.TagDirection

import           Control.Applicative            ( Applicative(liftA2) )
import           Control.Exception              ( throwIO )
import           Control.Monad                  ( join )
import           Control.Monad.Except           ( MonadError(throwError)
                                                , MonadIO(liftIO)
                                                )
import           Control.Monad.Reader           ( asks )
import qualified Data.Foldable                 as F
import           Data.HashSet                  as S
import qualified Data.List                     as L
import           Data.Maybe                     ( catMaybes )
import qualified Data.Text                     as T
import           Database.Persist               ( Entity(entityVal) )
import           MdGraph.App                    ( App
                                                , Env(config)
                                                )
import           MdGraph.App.Command
import           MdGraph.App.Logger             ( Logs
                                                , logDebug
                                                , logInfo
                                                )
import           MdGraph.Config                 ( Config
                                                  ( dbConnString
                                                  , libraryPath
                                                  )
                                                , HasConfig(getConfig)
                                                )
import           MdGraph.File                   ( Files
                                                , trueAbsolutePath
                                                )
import           MdGraph.Persist.Class          ( Queries(..) )
import           MdGraph.Persist.Query         as Q
import           MdGraph.Persist.Schema
import qualified MdGraph.Persist.Schema        as Edge
                                                ( Edge(..) )
import           System.Directory               ( canonicalizePath )
import           System.FilePath                ( makeRelative )



runCommand :: Command -> App [String]
runCommand Orphans             = fmap documentPath <$> runOrphans
runCommand Unreachable         = fmap documentPath <$> runUnreachable
runCommand Nonexes             = fmap edgeHead <$> runNonexistant
runCommand (Subgraph  options) = runSubgraph options
runCommand (Backlinks options) = runBacklinks options
runCommand Statics             = throwError "NYI"
runCommand Populate            = pure mempty

runOrphans :: (Monad m, Queries m, Logs m) => m [Document]
runOrphans = do
  orphanDocs <- getUnreachables
  logDebug $ T.unwords ["Found", T.pack . show . length $ orphanDocs, "orphans"]
  return $ entityVal <$> orphanDocs

runUnreachable :: (Monad m, Queries m, Logs m) => m [Document]
runUnreachable = do
  orphanDocs <- getUnreachables
  logDebug
    $ T.unwords ["Found", T.pack . show . length $ orphanDocs, "unreachable"]
  return $ entityVal <$> orphanDocs

runNonexistant :: (Monad m, Queries m, Logs m) => m [Edge]
runNonexistant = do
  nonexes <- getNonexistants
  logDebug
    $ T.unwords ["Found", T.pack . show . length $ nonexes, "nonexistant"]
  return $ entityVal <$> nonexes

runSubgraph
  :: (Monad m, Queries m, Logs m, Files m, HasConfig m)
  => SubgraphOptions
  -> m [FilePath]
runSubgraph options@SubgraphOptions { sgTargets, sgDepth } = do
  logInfo . T.unwords $ ["Finding subgraphs"]
  paths <- F.foldrM (flip $ runSubgraphOnArg getForwardLinks sgDepth)
                    S.empty
                    sgTargets
  return $ S.toList paths

type LinkGetter m = FilePath -> m [Entity Document]

runSubgraphOnArg
  :: (Monad m, HasConfig m, Files m)
  => LinkGetter m
  -> Integer
  -> HashSet FilePath
  -> SubgraphTarget
  -> m (HashSet FilePath)
runSubgraphOnArg linkGetter maxDepth foundPaths (FileTarget path) = do
  libPath <- libraryPath <$> getConfig
  absPath <- trueAbsolutePath path
  let relPath = makeRelative libPath absPath
  -- TODO: fix infinite depth to be a real value instead of this hack
  runSubgraphPath' linkGetter maxDepth 0 foundPaths relPath

runSubgraphOnArg _ _ _ _ = pure S.empty -- TODO

runSubgraphPath'
  :: (Monad m)
  => LinkGetter m
  -> Integer
  -> Integer
  -> S.HashSet FilePath
  -> FilePath
  -> m (HashSet FilePath)
runSubgraphPath' linkGetter maxDepth currentDepth foundPaths newPath = do
  let alreadyExists = S.member newPath foundPaths
      pastMaxDepth  = currentDepth == maxDepth
  if alreadyExists || pastMaxDepth
    then pure foundPaths
    else do
      children <- linkGetter newPath
      let setWithCurrent = S.insert newPath foundPaths
      let childPaths     = documentPath . entityVal <$> children
      F.foldrM
        (flip $ runSubgraphPath' linkGetter maxDepth (currentDepth + 1))
        setWithCurrent
        childPaths

runBacklinks :: BacklinkOptions -> App [FilePath]
runBacklinks options@BacklinkOptions { blTargets, blDepth } = do
  logInfo . T.unwords $ ["Finding backlinks"]
  paths <- F.foldrM (flip $ runSubgraphOnArg getBackwardLinks blDepth)
                    S.empty
                    blTargets
  return $ S.toList paths
