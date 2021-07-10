module Main where

import           Data.Text                     as T
import           Data.Text.IO                  as T
import           Lib
import           System.Environment
import           Text.Pandoc.Class
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import           Text.Pandoc.Readers
import           Text.Pandoc.Walk

miniMarkdown :: Text
miniMarkdown = "foo bar baz"
minimalPandoc :: Text -> IO Pandoc
minimalPandoc = runIOorExplode . readMarkdown def

main :: IO ()
main = do
  content <- T.readFile "/home/elpfen/zk/index.md"
  ast     <- runIOorExplode . readVimwiki def $ content
  print ast
  let result = query extractUrl ast
  print result

extractUrl :: Inline -> [Text]
extractUrl (Link  _ _ (u, _)) = [u]
extractUrl (Image _ _ (u, _)) = [u]
extractUrl _                  = []
