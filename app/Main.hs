module Main where

import           Aux.Map                       as M
import Aux.Common
import MdGraph
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
import           MdGraph.App
import           MdGraph.Config

import           Control.Concurrent.Async       ( mapConcurrently )
import           Control.Monad                  ( forM
                                                , void
                                                )
import           Control.Monad.Except           ( runExceptT )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Reader           ( ReaderT(runReaderT) )
import           Data.Foldable                 as F
                                                ( mapM_ )
import           Data.HashSet                  as S
                                                ( fromList
                                                , toList
                                                )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( catMaybes )
import           Data.Text                     as T
import           Data.Text.IO                     as T
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
    args <- cliArguments
    conf <- runExceptT $ argsToConfig args
    print conf
    -- TODO: Better error handling here
    forEither conf (T.putStrLn)
        $ \conf -> do
           let env = Env conf
           out <- runExceptT (runReaderT (runApp prepareDatabase) env)
           either T.putStrLn (const $ pure ()) out
