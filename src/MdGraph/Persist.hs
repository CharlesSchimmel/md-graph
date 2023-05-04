{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-#  LANGUAGE TypeApplications  #-}
{-#  LANGUAGE RankNTypes  #-}

module MdGraph.Persist where

import           MdGraph.App.Arguments          ( DatabaseArg(DbFile, Temp) )

import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import qualified Data.Text                     as T
import qualified Data.Text                     as T
import           GHC.IO.Handle                  ( hIsWritable )
import           MdGraph.File.Internal          ( trueAbsolutePathIO )
import qualified System.Directory              as D
import           System.IO                      ( IOMode
                                                    ( ReadWriteMode
                                                    , WriteMode
                                                    )
                                                , withFile
                                                )
import           System.Posix.Temp

dbArgToConnString :: DatabaseArg -> IO (Maybe T.Text)
dbArgToConnString Temp = do
    (path, handle) <- mkstemp "mdgraph"
    return . Just $ T.pack path

dbArgToConnString (DbFile path) = do
    absPath <- T.pack <$> trueAbsolutePathIO (T.unpack path)
    -- TODO:
    -- If it's a new file, then attempt creating an empty file
    -- Check if file can be Read-Written
    exists  <- liftIO $ withFile (T.unpack absPath) ReadWriteMode hIsWritable
    return $ if exists then Just absPath else Nothing
