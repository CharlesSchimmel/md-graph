module MdGraph.File
    ( Files(..)
    ) where

import           Control.Applicative
import           Control.Concurrent.Async       ( mapConcurrently )
import           Control.Monad                  ( join )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Reader           ( asks )
import           Control.Monad.Trans.Maybe
import           Data.Foldable
import           Data.Hashable                  ( Hashable )
import qualified Data.List
import           Data.Maybe
import           Data.Text                     as T
                                                ( pack
                                                , unwords
                                                )
import           Data.Time                      ( UTCTime )
import           Data.Traversable              as T
import           MdGraph.App                    ( App(App) )
import           Prelude                       as P
import           System.Directory              as D
import           System.FilePath               as F

import           MdGraph.File.Internal

class Files m where
  trueAbsolutePath :: FilePath -> m FilePath

instance Files App where
    trueAbsolutePath = liftIO . trueAbsolutePathIO
