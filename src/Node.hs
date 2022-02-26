{-# LANGUAGE DeriveGeneric #-}
module Node where

import           Data.Hashable
import           Data.Text                     as T
import           GHC.Generics                   ( Generic )

data Node = Link { linkPath :: FilePath } | Tag { tagLabel :: Text }
    deriving (Eq, Generic, Show)

instance Hashable Node where

printNode :: Node -> String
printNode (Tag  text) = T.unpack . T.concat $ ["#", text]
printNode (Link path) = path

nodePath :: Node -> Maybe FilePath
nodePath (Link l) = Just l
nodePath _        = Nothing

tagText :: Node -> Maybe Text
tagText (Tag t) = Just t
tagText _       = Nothing
