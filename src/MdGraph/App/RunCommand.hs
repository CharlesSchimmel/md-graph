{-# LANGUAGE  NamedFieldPuns  #-}
module MdGraph.App.RunCommand where

import           Aux.HashSet
import           MdGraph.TagDirection

import           Control.Applicative            ( Applicative(liftA2) )
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
import           MdGraph.App.Logger             ( logDebug
                                                , logInfo
                                                )
import           MdGraph.Config                 ( Config
                                                  ( dbConnString
                                                  , libraryPath
                                                  )
                                                )
import           MdGraph.File                   ( trueAbsolutePath )
import           MdGraph.Persist.Query         as Q
import           MdGraph.Persist.Schema
import qualified MdGraph.Persist.Schema        as Edge
                                                ( Edge(..) )
import           MdGraph.Util                   ( trace'' )
import           System.Directory               ( canonicalizePath )
import           System.FilePath                ( makeRelative )



runCommand :: Command -> App [String]
runCommand Orphans             = fmap documentPath <$> runOrphans
runCommand Unreachable         = fmap documentPath <$> runUnreachable
runCommand Nonexes             = fmap edgeHead <$> runNonexistant
runCommand Statics             = throwError "NYI"
runCommand (Subgraph  options) = runSubgraph options
runCommand (Backlinks options) = throwError "NYI"
runCommand Populate            = pure mempty

runOrphans :: App [Document]
runOrphans = do
  connString <- asks $ dbConnString . config
  orphanDocs <- Q.orphans connString
  let orphanCount = length orphanDocs
  logDebug $ T.unwords ["Found", T.pack . show $ orphanCount, "orphans"]
  return $ entityVal <$> orphanDocs

runUnreachable :: App [Document]
runUnreachable = do
  connString <- asks $ dbConnString . config
  orphanDocs <- Q.unreachable connString
  let orphanCount = length orphanDocs
  logDebug $ T.unwords ["Found", T.pack . show $ orphanCount, "unreachable"]
  return $ entityVal <$> orphanDocs

runNonexistant :: App [Edge]
runNonexistant = do
  connString <- asks $ dbConnString . config
  nonexes    <- Q.nonexistant connString
  let count = length nonexes
  logDebug $ T.unwords ["Found", T.pack . show $ count, "nonexistant"]
  return $ entityVal <$> nonexes

runSubgraph :: SubgraphOptions -> App [FilePath]
runSubgraph options@SubgraphOptions { sgTargets } = do
  logInfo . T.unwords $ ["Finding subgraphs"]
  -- let protoResults = runSubgraphOnArg <$> sgTargets
  paths <- F.foldrM (flip runSubgraphOnArg) S.empty sgTargets
  return $ S.toList paths

runSubgraphOnArg
  :: HashSet FilePath -> SubgraphTarget -> App (HashSet FilePath)
runSubgraphOnArg foundPaths (FileTarget path) = do
  libPath <- asks $ libraryPath . config
  absPath <- liftIO $ trueAbsolutePath path
  logDebug . T.unwords $ ["Abs path", T.pack absPath]
  let relPath =
        trace'' "Subgraph target relative path" $ makeRelative libPath absPath
  runSubgraphPath' foundPaths relPath

runSubgraphOnArg _ _ = throwError "Tag subgraph not yet implemented"

runSubgraphPath' :: S.HashSet FilePath -> FilePath -> App (HashSet FilePath)
runSubgraphPath' foundPaths newPath = do
  let alreadyExists = S.member newPath foundPaths
  if alreadyExists
    then pure foundPaths
    else do
      connString <- asks $ dbConnString . config
      children   <- Q.forwardLinks connString newPath
      let setWithCurrent = S.insert newPath foundPaths
      let childPaths     = documentPath . entityVal <$> catMaybes children
      F.foldrM (flip runSubgraphPath') setWithCurrent childPaths
