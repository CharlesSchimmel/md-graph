{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module FilesSpec where

import Control.Exception (evaluate)
import qualified Control.Exception as E
import Control.Monad (unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Data.Either (fromRight, isRight)
import qualified Data.Text as T
import Database.Persist.Sqlite (runSqlPersistM, wrapConnection)
import Database.Sqlite (open)
import MdGraph (mdGraph)
import qualified MdGraph
import MdGraph.App (App (runApp), Env (Env))
import MdGraph.App.Arguments (Arguments (..))
import qualified MdGraph.App.Arguments as Arguments
import MdGraph.App.Command (BacklinkOptions (..), Command (..), SubgraphOptions (..), SubgraphTarget (..))
import qualified MdGraph.App.Command as Command
import qualified MdGraph.App.LogLevel as LogLevel
import MdGraph.App.RunCommand (runCommand)
import MdGraph.Config (Config (Config, libraryPath))
import MdGraph.File (Files (..), isAncestorOf, normaliseEvil, unrelativize)
import MdGraph.File.Types (AbsolutePath (..), File)
import MdGraph.Node (Link (..))
import qualified MdGraph.TagDirection as TagDirection
import Spec.Base
import System.Directory (getCurrentDirectory)
import System.FilePath
import Test.Hspec
import Test.Hspec.Contrib.HUnit
import Test.Hspec.QuickCheck
import Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Files" $ do
    describe "normaliseEvil" $ do
      it "Parent traversals in the middle of paths are handled" $ do
        normaliseEvil (AbsolutePath "/foo/bar/baz/../qux.md") `shouldBe` AbsolutePath "/foo/bar/qux.md"
      it "Two parent traversals in the middle of paths are handled" $ do
        normaliseEvil (AbsolutePath "/foo/bar/baz/../../qux.md") `shouldBe` AbsolutePath "/foo/qux.md"
      it "Three parent traversals in the middle of paths are handled" $ do
        normaliseEvil (AbsolutePath "/foo/bar/baz/../../../qux.md") `shouldBe` AbsolutePath "/qux.md"
      it "Same-directory traversals in the middle of paths are handled" $ do
        normaliseEvil (AbsolutePath "/foo/./baz/qux.md") `shouldBe` AbsolutePath "/foo/baz/qux.md"
      it "Same-directory and multiple parent traversals in the middle of paths are handled" $ do
        normaliseEvil (AbsolutePath "/foo/bar/./baz/.././../qux.md") `shouldBe` AbsolutePath "/foo/qux.md"
      it "Parent traversal to root is handled" $ do
        normaliseEvil (AbsolutePath "/foo/../bar.md") `shouldBe` AbsolutePath "/bar.md"
      it "Parent traversal stops at root" $ do
        normaliseEvil (AbsolutePath "/foo/../../../bar.md") `shouldBe` AbsolutePath "/bar.md"
      it "Parent traversal stops at root (with an even number of traversals)" $ do
        normaliseEvil (AbsolutePath "/foo/../../../../bar.md") `shouldBe` AbsolutePath "/bar.md"
      it "Circular traversals are simplified" $ do
        normaliseEvil (AbsolutePath "/foo/../foo/../foo/../foo/../bar.md") `shouldBe` AbsolutePath "/bar.md"
    -- TODO: some of the cases from https://hackage.haskell.org/package/filepath-1.5.4.0/docs/System-FilePath.html#v:normalise

    it "Unrelativize" $ do
      unrelativize (AbsolutePath "/foo/bar/baz.md") "./qux.md" `shouldBe` AbsolutePath "/foo/bar/qux.md"
      unrelativize (AbsolutePath "/foo/bar/baz.md") "../qux.md" `shouldBe` AbsolutePath "/foo/qux.md"
      unrelativize (AbsolutePath "/foo/bar/baz.md") "./subdir/qux.md" `shouldBe` AbsolutePath "/foo/bar/subdir/qux.md"
      unrelativize (AbsolutePath "/foo/bar/baz.md") "../subdir/qux.md" `shouldBe` AbsolutePath "/foo/subdir/qux.md"
      unrelativize (AbsolutePath "/foo/bar/baz.md") "./qux.md" `shouldBe` AbsolutePath "/foo/bar/qux.md"

    it "isAncestorOf" $ do
      isAncestorOf (AbsolutePath "/foo/bar") (AbsolutePath "/foo/bar/baz.md") `shouldBe` True
      isAncestorOf (AbsolutePath "/foo") (AbsolutePath "/foo/bar/baz.md") `shouldBe` True
      isAncestorOf (AbsolutePath "/foo/qux/whiz") (AbsolutePath "/foo/bar/baz.md") `shouldBe` False
      isAncestorOf (AbsolutePath "/qux") (AbsolutePath "/foo/bar/baz.md") `shouldBe` False
