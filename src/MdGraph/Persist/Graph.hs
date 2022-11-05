{-# LANGUAGE TypeApplications #-}
module MdGraph.Persist.Graph where

import           MdGraph.Persist.Schema

import           Database.Esqueleto.Experimental
import           Database.Persist.Sqlite        ( runSqlite )

backLinks connString file = runSqlite connString $ select $ do
    (backLink :& edge :& source) <-
        from
        $           table @Document
        `innerJoin` table @Edge
        `on`        (\(doc :& edge) -> doc ^. DocumentId ==. edge ^. EdgeTail)
        `innerJoin` table @Document
        `on` (\(_ :& edge :& doc) -> doc ^. DocumentPath ==. edge ^. EdgeHead)
    -- where_ source ^. DocumentPath ==. val file
    return backLink

-- | subgraph: WithRecursive the Edge table to get the full subgraph, then
-- traverse that to get the limited subgraph
