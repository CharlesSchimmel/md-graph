module Main where

import           Aux.Map                       as M
import           MdGraph.App.Arguments
import           MdGraph.App.Logger
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
import           Data.Text                     as T
import           Database.Persist.Sqlite        ( Entity(entityVal)
                                                , runSqlite
                                                )
import           Options.Applicative
import           Prelude
import           Prelude                       as P
                                                ( foldr
                                                , length
                                                , map
                                                , print
                                                , putStrLn
                                                )

main :: IO ()
main = do
    Arguments {..} <- opts

    logDebug "Preparing database"
    migrateMdGraph argDatabase

    -- find documents
    logDebug "Finding documents"
    docs <- findDocuments argDefExt [argLibrary]
    logInfo
        . T.unwords
        $ ["Found", T.pack . show . P.length $ docs, "documents"]

    -- load documents into temp
    logDebug "Populating TempDocuments"
    insertTempDocuments argDatabase $ Mapper.fromFile <$> docs

    logDebug "Pruning deleted Documents"
    pruneDeletedDocuments argDatabase

    logDebug "Pruning unchanged TempDocuments"
    pruneUnchangedTempDocs argDatabase

    logDebug "Pruning modified Documents"
    pruneModifiedDocs argDatabase

    -- find new (including "new" meaning modified)
    logDebug "Finding new and modified TempDocuments"
    newTempDocs <- newFiles argDatabase
    let docsToInsert = fromTempDocument . entityVal <$> newTempDocs

    logDebug "Inserting new and modified Documents"
    newDocs <- insertDocuments argDatabase docsToInsert

    let docKeyMap   = M.flop documentPath newDocs
        docsToParse = M.keys docKeyMap

    logDebug "Parsing new and modified document"
    -- TODO: Need to fix link targets, missing extensions
    parseResults <-
        catMaybes
            <$> mapConcurrently (parseDocument argDefExt argLibrary) docsToParse
    let resultMap = M.fromList' file parseResults
        pathToKeyResult =
            P.map snd . M.toList $ M.unionWith' docKeyMap resultMap
        newTagsAndEdges :: [([Tag], [Edge])]
        newTagsAndEdges = P.map
            (\(docKey, result) ->
                (fromParseResult docKey result, fromParseResult docKey result)
            )
            pathToKeyResult
        (newTags, newEdges) = P.foldr
            (\(accTags, accEdges) (tags, edges) ->
                (accTags ++ tags, accEdges ++ edges)
            )
            ([], [])
            newTagsAndEdges

    -- P.print newEdges
    logDebug "Inserting new edges"
    insertEdges argDatabase newEdges

    logDebug "Inserting new tags"
    insertTags argDatabase newTags

    P.putStrLn "Quack"
