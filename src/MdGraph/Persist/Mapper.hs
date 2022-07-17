{-# LANGUAGE FlexibleInstances #-}
module MdGraph.Persist.Mapper where

import qualified Data.Text                     as T
import           Data.Traversable               ( for )
import qualified MdGraph.File                  as Dto
import           MdGraph.Parse                  ( ParseResult(..) )
import           MdGraph.Persist.Schema

class FromFile a where
  fromFile :: Dto.File -> a

instance FromFile Document where
    fromFile Dto.File { filePath, modificationTime } =
        Document filePath modificationTime

instance FromFile TempDocument where
    fromFile Dto.File { filePath, modificationTime } =
        TempDocument filePath modificationTime

class FromTempDocument a where
  fromTempDocument :: TempDocument -> a

instance FromTempDocument Document where
    fromTempDocument TempDocument { tempDocumentPath, tempDocumentModifiedAt }
        = Document tempDocumentPath tempDocumentModifiedAt

class FromParseResult a where
  fromParseResult :: Key Document -> ParseResult -> a

instance FromParseResult [Edge] where
    fromParseResult docKey ParseResult { links } = map (Edge docKey) links

instance FromParseResult [Tag] where
    fromParseResult docKey ParseResult { tags } =
        map ((\tag -> Tag tag docKey) . T.unpack) $ tags
