module Jinzamomi.Driver.Util (
  enumAllFiles
) where
--

import System.Directory
import System.FilePath (pathSeparator)
import Control.Monad (filterM)
import Data.List.Utils (endswith)

--
enumAllFiles :: FilePath -> IO [FilePath]
enumAllFiles path = do
    allItems <- getDirectoryContents path
    let items = (\f -> path ++ pathSeparator:f) <$> filter (\k -> k /= "." && k /= "..") allItems
    files <- filterM doesFileExist items
    justFolders <- filterM doesDirectoryExist items
    leftFiles <- mapM enumAllFiles justFolders
    return (files ++ concat leftFiles)
