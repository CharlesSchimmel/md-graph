module Main where

import           Aux.Map                       as M
import           MdGraph.App.Arguments
import           MdGraph.App.Command            ( runCommand )
import           MdGraph.File                   ( findDocuments )
import           MdGraph.Node                   ( printNode )
import           MdGraph.Parse                  ( ParseResult(..)
                                                , parseDocuments
                                                )
import           MdGraph.Persist
import           MdGraph.Persist.Mapper        as Mapper
import           MdGraph.Persist.Schema         ( Document(documentPath)
                                                , Edge(..)
                                                , Tag(..)
                                                , TempDocument(tempDocumentPath)
                                                , migrateMdGraph
                                                )

import           Control.Monad                  ( forM )
import           Data.Foldable                 as F
                                                ( mapM_ )
import           Data.HashSet                  as S
                                                ( fromList
                                                , toList
                                                )
import qualified Data.Map.Strict               as M
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

main :: IO ()
main = do
    Arguments {..} <- opts
    -- find documents
    docs           <- findDocuments argDefExt argLibrary
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
    parseResults <- parseDocuments argDefExt docsToParse
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

    insertEdges argDatabase newEdges
    insertTags argDatabase newTags

    P.print docs
    -- links   <- parseDocuments (argDefExt args) docs
    P.putStrLn "Quack"
    -- results <- runCommand (argCommand args) links $ S.fromList links
    -- F.mapM_ P.putStrLn . P.map printNode . S.toList $ results
