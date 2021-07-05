module Link where

import           Data.Text

data Link = Link
  { uri   :: Text
  , title :: Text
  }
  deriving Show

