module Main where

import           MdGraph.App.Arguments
import           MdGraph.App.Command            ( runCommand )
import           MdGraph.File                   ( retrieveFiles )
import           MdGraph.Node                   ( printNode )
import           MdGraph.Parse                  ( parseNodes )

import           Data.Foldable                 as F
                                                ( mapM_ )
import           Data.HashSet                  as S
                                                ( fromList
                                                , toList
                                                )
import           Options.Applicative
import           Prelude
import           Prelude                       as P
                                                ( map
                                                , putStrLn
                                                )

main :: IO ()
main = do
    args    <- opts
    files   <- retrieveFiles (argDefExt args) (argLibrary args)
    links   <- parseNodes (argDefExt args) files
    results <- runCommand (argCommand args) links $ S.fromList files
    F.mapM_ P.putStrLn . P.map printNode . S.toList $ results
