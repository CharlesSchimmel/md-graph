module MdGraph.Parse.Types where

import Data.Char (toLower)
import System.FilePath (takeExtension)

data DocumentFormat = Markdown | MediaWiki
  deriving
    ( -- | OrgMode
      Eq,
      Ord,
      Show
    )

inferDocumentFormat :: FilePath -> Maybe DocumentFormat
inferDocumentFormat path =
  case ext of
    ".md" -> Just Markdown
    -- "org" -> Just OrgMode
    ".wiki" -> Just MediaWiki
    _ -> Nothing
  where
    ext = toLower <$> takeExtension path
