module Jinzamomi.Driver.Util (
  enumAllFiles
) where
--

import System.Directory
import Control.Monad (filterM)
import Data.List.Utils (endswith)
import System.FilePath (pathSeparator, makeRelative, normalise)

--
enumAllFiles :: FilePath -> IO [FilePath]
enumAllFiles path = do
    files <- enumAllFiles' path
    return (fmap (makeRelative path) files)
  where
    enumAllFiles' path = do
      let normpath = normalise path
      allItems <- getDirectoryContents path
      let items = (\f -> path ++ pathSeparator:f) <$> filter (\k -> k /= "." && k /= "..") allItems
      files <- filterM doesFileExist items
      justFolders <- filterM doesDirectoryExist items
      leftFiles <- mapM enumAllFiles' justFolders
      return (files ++ concat leftFiles)
