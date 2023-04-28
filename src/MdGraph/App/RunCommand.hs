{-# LANGUAGE  NamedFieldPuns  #-}
module MdGraph.App.RunCommand where

import           Aux.HashSet
import           MdGraph.TagDirection

import           Control.Monad                  ( join )
import           Control.Monad.Except           ( MonadError(throwError)
                                                , MonadIO(liftIO)
                                                )
import           Control.Monad.Reader           ( asks )
import           Data.HashSet                  as S
import           Data.Maybe                     ( catMaybes )
import qualified Data.Text                     as T
import           Database.Persist               ( Entity(entityVal) )
import           MdGraph.App                    ( App
                                                , Env(config)
                                                )
import           MdGraph.App.Command
import           MdGraph.App.Logger             ( logDebug )
import           MdGraph.Config                 ( Config(dbConnString) )
import           MdGraph.Persist.Query         as Q
import           MdGraph.Persist.Schema
import qualified MdGraph.Persist.Schema        as Edge
                                                ( Edge(..) )



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
runSubgraph options@SubgraphOptions { sgTargets } =
  join <$> mapM runSubgraph' sgTargets

runSubgraph' :: SubgraphTarget -> App [FilePath]
runSubgraph' (FileTarget path) = runSubgraphPath path
runSubgraph' _                 = throwError "NYI"

runSubgraphPath :: FilePath -> App [FilePath]
runSubgraphPath path = do
  connString <- asks $ dbConnString . config
  nonexes    <- Q.forwardLinks connString path
  let paths = documentPath . entityVal <$> catMaybes nonexes
  recurse <- join <$> mapM runSubgraphPath paths
  return $ path : recurse
