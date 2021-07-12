module Options where

import           System.FilePath

data Arguments = Arguments
    { argLibrary :: [FilePath]
    , argDefExt  :: FilePath
    , argRunType :: RunType
    }

data RunType = Subgraph { subgraphRoot :: FilePath } | Orphans | Stranded
