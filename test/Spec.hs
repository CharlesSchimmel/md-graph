import           MdGraph.File.Internal
import           System.FilePath

main :: IO ()
main = do
    print $ aoeu "/foo/bar/baz/bang" "../../blah"
    putStrLn "Test suite not yet implemented"

aoeu source dest = trueDest </> joinDir absoluteParts
  where
    sourceParts    = splitDirectories source
    destParts      = splitDirectories dest
    isRelativePart = (== "..")
    relativeParts  = length . takeWhile isRelativePart $ destParts
    absoluteParts  = dropWhile isRelativePart $ destParts
    trueDest = joinDir $ reverse . drop relativeParts . reverse $ sourceParts
