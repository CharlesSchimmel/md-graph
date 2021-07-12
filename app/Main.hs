module Main where

import           Lib
import           Options

import           Data.HashMap.Lazy             as M
import           Data.HashSet                  as S
import           Data.Text                     as T
import           Data.Text.IO                  as T
import           Options.Applicative
import           Prelude                       as P
import           System.Environment
import           Text.Pandoc.Class
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import           Text.Pandoc.Readers
import           Text.Pandoc.Walk

main :: IO ()
main = do
    args    <- execParser opts
    corpus' <- corpus (argDefExt args) (argLibrary args)
    let runResult = doRun corpus' (argRunType args)
    P.mapM_ P.putStrLn runResult

doRun :: Corpus -> RunType -> [FilePath]
doRun (Corpus fwdMap bwdMap allFiles) Orphans =
    S.toList $ orphans fwdMap bwdMap allFiles

doRun (Corpus fwdMap bwdMap allFiles) Unreachable =
    S.toList $ stranded fwdMap bwdMap

doRun (Corpus fwdMap _ _) (Subgraph file) = S.toList $ subgraph fwdMap file

doRun (Corpus _ bwdMap _) (Backlinks file) =
    maybe [] S.toList $ bwdMap M.!? file
