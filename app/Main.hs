module Main where

import           Lib
import           Options

import           Data.HashMap.Lazy             as M
import           Data.HashSet                  as S
import           Data.Maybe
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
    w@Weirdos { statix, nonex } <- weirdos corpus'
    let modStatic = if argIncStatic args
            then runResult
            else runResult `S.difference` statix
        modNonex = if argIncNonex args
            then modStatic
            else modStatic `S.difference` nonex
    P.mapM_ P.putStrLn modNonex

doRun :: Corpus -> RunType -> HashSet FilePath
doRun (Corpus fwdMap bwdMap allFiles) Orphans = orphans fwdMap bwdMap allFiles

doRun (Corpus fwdMap bwdMap allFiles) Unreachable = stranded fwdMap bwdMap

doRun (Corpus fwdMap _ _) (Subgraph file) = subgraph fwdMap file

doRun (Corpus _ bwdMap _) (Backlinks file) =
    fromMaybe S.empty $ bwdMap M.!? file
