module Main where

import Aux.Common
import Aux.Map as M
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad
  ( forM,
    join,
    void,
  )
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Foldable as F
  ( mapM_,
  )
import Data.HashSet as S
  ( fromList,
    toList,
  )
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Text as T
import qualified Data.Text.IO as T
import Database.Persist.Sqlite (wrapConnection)
import Database.Sqlite (open)
import MdGraph
import MdGraph.App
import MdGraph.App.Arguments
import MdGraph.App.Logger
import MdGraph.Config
import MdGraph.Persist
import MdGraph.Persist.Mapper as Mapper
import MdGraph.Persist.Schema
  ( Document (documentPath),
    Edge (..),
    Tag (..),
    TempDocument (tempDocumentPath),
    migrateMdGraph,
  )
import Options.Applicative
import Prelude
import Prelude as Prelude
  ( foldr,
    length,
    map,
    print,
    putStrLn,
  )

main :: IO ()
main = do
  args@Arguments {..} <- cliArguments
  out <- mdGraph args
  either T.putStrLn (F.mapM_ Prelude.putStrLn) out
