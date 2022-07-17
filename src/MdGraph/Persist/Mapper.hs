module MdGraph.Persist.Mapper where

import qualified MdGraph.File                  as Dto
import qualified MdGraph.Persist.Schema        as Persist

class FromFile a where
  fromFile :: Dto.File -> a

instance FromFile Persist.Document where
    fromFile Dto.File { filePath, modificationTime } =
        Persist.Document filePath modificationTime

instance FromFile Persist.TempDocument where
    fromFile Dto.File { filePath, modificationTime } =
        Persist.TempDocument filePath modificationTime

