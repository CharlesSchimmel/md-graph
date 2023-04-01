module MdGraph.App.RunCommand where

import           Aux.HashSet
import           MdGraph.TagDirection

import           Control.Monad.Except           ( MonadError(throwError)
                                                , MonadIO(liftIO)
                                                )
import           Control.Monad.Reader           ( asks )
import           Data.HashSet                  as S
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



runCommand :: Command -> App [Document]
runCommand Orphans = runOrphans
runCommand _       = throwError "Not yet implemented"

runOrphans :: App [Document]
runOrphans = do
    connString <- asks $ dbConnString . config
    orphanDocs <- Q.orphans connString
    let orphanCount = length orphanDocs
    logDebug $ T.unwords ["Found", T.pack . show $ orphanCount, "orphans"]
    return $ entityVal <$> orphanDocs


-- runCommand
--     :: Command -> [(Node, Node)] -> HashSet FilePath -> IO (HashSet Node)

-- runCommand Orphans nodes allFiles = return $ orphans fwdMap bwdMap allFiles
--     where (fwdMap, bwdMap) = buildMaps nodes

-- runCommand Unreachable nodes allFiles = return $ stranded fwdMap bwdMap
--     where (fwdMap, bwdMap) = buildMaps nodes

-- runCommand Nonexes nodes allFiles = do
--     let (fwdMap, bwdMap) = buildMaps nodes
--     results <- weirdos $ Corpus fwdMap bwdMap allFiles
--     return $ nonex results

-- runCommand Statics nodes allFiles = do
--     let (fwdMap, bwdMap) = buildMaps nodes
--     results <- weirdos $ Corpus fwdMap bwdMap allFiles
--     return $ statix results

-- runCommand (Subgraph (SubgraphOptions targets incNonex incStatic tagDir depth)) nodes allFiles
--     = do
--         (Weirdos statix nonex) <- weirdos $ Corpus fwdGraph bwdGraph allFiles
--         return . withNonex nonex . withStatic statix $ subgraph depth
--                                                                 fwdGraph
--                                                                 targets
--   where
--     (fwdGraph, bwdGraph) = buildMaps $ adjustTagDir tagDir nodes
--     withNonex            = if incNonex then const id else flip S.difference
--     withStatic           = if incStatic then const id else flip S.difference

-- runCommand (Backlinks (BacklinkOptions targets depth)) nodes allFiles =
--     return $ subgraph depth bwdGraph targets
--     where (fwdGraph, bwdGraph) = buildMaps nodes
