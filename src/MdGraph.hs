module MdGraph where

import           Aux.Map                       as M
import           MdGraph.App.Arguments
import           MdGraph.File                   ( findDocuments )
import           MdGraph.Parse                  ( ParseResult(..)
                                                , parseDocument
                                                )
import           MdGraph.Persist
import           MdGraph.Persist.Mapper        as Mapper
import           MdGraph.Persist.Schema         ( Document(documentPath)
                                                , Edge(..)
                                                , Tag(..)
                                                , TempDocument(tempDocumentPath)
                                                , migrateMdGraph
                                                )

import           Control.Concurrent.Async       ( mapConcurrently )
import           Control.Monad                  ( forM )
import           Data.Foldable                 as F
                                                ( mapM_ )
import           Data.HashSet                  as S
                                                ( fromList
                                                , toList
                                                )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( catMaybes )
import           Database.Persist.Sqlite        ( Entity(entityVal)
                                                , runSqlite
                                                )
import           Options.Applicative
import           Prelude
import           Prelude                       as P
                                                ( map
                                                , print
                                                , putStrLn
                                                )

prepareDatabase :: Arguments -> IO ()
prepareDatabase Arguments {..} = do
    -- find documents
    docs <- findDocuments argDefExt [argLibrary]
    migrateMdGraph argDatabase
    -- load documents into temp
    insertTempDocuments argDatabase $ Mapper.fromFile <$> docs

    pruneDeletedDocuments argDatabase
    pruneUnchangedTempDocs argDatabase
    pruneModifiedDocs argDatabase

    -- find new (including "new" meaning modified)
    newTempDocs <- newFiles argDatabase
    let docsToInsert = fromTempDocument . entityVal <$> newTempDocs

    newDocs <- insertDocuments argDatabase docsToInsert
    let docKeyMap   = M.flop documentPath newDocs
        docsToParse = M.keys docKeyMap
    parseResults <-
        catMaybes
            <$> mapConcurrently (parseDocument argDefExt argLibrary) docsToParse
    let resultMap = M.fromList' file parseResults
        pathToKeyResult =
            Prelude.map snd . M.toList $ M.unionWith' docKeyMap resultMap
        newTagsAndEdges :: [([Tag], [Edge])]
        newTagsAndEdges = Prelude.map
            (\(docKey, result) ->
                (fromParseResult docKey result, fromParseResult docKey result)
            )
            pathToKeyResult
        (newTags, newEdges) = Prelude.foldr
            (\(accTags, accEdges) (tags, edges) ->
                (accTags ++ tags, accEdges ++ edges)
            )
            ([], [])
            newTagsAndEdges

    -- P.print newEdges
    insertEdges argDatabase newEdges
    insertTags argDatabase newTags

    P.putStrLn "Quack"
