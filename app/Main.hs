module Main where

import           MdGraph.App.Arguments
import           MdGraph.App.Command            ( runCommand )
import           MdGraph.File                   ( findDocuments )
import           MdGraph.Node                   ( printNode )
import           MdGraph.Parse                  ( parseDocuments )
import           MdGraph.Persist                ( insertTempDocuments, newFiles, modifiedFiles, deletedFiles )
import           MdGraph.Persist.Schema         ( migrateMdGraph )
import           MdGraph.Persist.Mapper         as Mapper (FromFile(..))

import           Data.Foldable                 as F
                                                ( mapM_ )
import           Data.HashSet                  as S
                                                ( fromList
                                                , toList
                                                )
import           Database.Persist.Sqlite        ( runSqlite )
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
    -- find new, modified, deleted documents
    -- prune deleted documents and their edges
    -- pruned modified documents edges
    -- parse new documents and insert
    newDocs <- newFiles argDatabase
    modifiedDocs <- modifiedFiles argDatabase
    deletedDocs <- deletedFiles argDatabase
    P.print docs
    -- links   <- parseDocuments (argDefExt args) docs
    P.putStrLn "Quack"
    -- results <- runCommand (argCommand args) links $ S.fromList links
    -- F.mapM_ P.putStrLn . P.map printNode . S.toList $ results
