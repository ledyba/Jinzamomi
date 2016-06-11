module Jinzamomi.Driver.Krkr (
  Opt,
  opt,
  execute
) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as JSON
import qualified Text.Parsec.Error as P
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified　Language.TJS as TJS
import qualified　Language.KAG as KAG
import System.Log.Logger
import Options.Applicative
import qualified Jinzamomi.Driver.Krkr.TJS2IR as TJS2IR
import qualified Jinzamomi.Driver.Krkr.KAG2IR as KAG2IR
import qualified Jinzamomi.Driver.IR as IR
import Jinzamomi.Driver.Util
import Control.Monad (filterM)
import System.FilePath (dropFileName, takeExtension, (</>), (<.>), pathSeparator, makeRelative, normalise)
import System.Directory (createDirectoryIfMissing, getDirectoryContents, doesFileExist, doesDirectoryExist)

data Opt =
    Build String String
  deriving (Show)

tag :: String
tag = "Krkr"

opt :: Mod CommandFields Opt
opt = command "krkr" (info buildCmd (progDesc "Krkr Driver."))
  where
    buildCmd = hsubparser $
      command "build" (
        info buildOption
        (progDesc "Build krkr"))
    buildOption = Build <$>
            argument str (metavar "FROM") <*>
            argument str (metavar "TO")

compileTJS :: FilePath -> T.Text -> Either P.ParseError T.Text
compileTJS filepath content = do
  ast <- TJS.parse filepath content
  return (IR.compile (TJS2IR.compileStmt ast))

compileKAG :: FilePath -> T.Text -> Either P.ParseError T.Text
compileKAG filepath content = do
  ast <- KAG.parse filepath content
  return (IR.compile (KAG2IR.compile ast))

build :: FilePath -> FilePath -> IO ()
build from to = do
    createDirectoryIfMissing True to
    files <- enumAllFiles from
    outFileList from to from
    results <- mapM run files
    let errors = length (filter not results)
    if errors /= 0 then
      errorM tag (concat ["compiling errors on ", show errors, "(/", show (length results), ") tjs2 files. done."])
    else
      return ()
  where
    runTask path ext runner = do
      let inPath = from </> path
      content <- TIO.readFile inPath
      case runner inPath content of
        Right res -> do
          infoM tag ("[OK] " ++ inPath)
          let outPath = to </> path <.> ext
          createDirectoryIfMissing True (dropFileName outPath)
          TIO.writeFile outPath res
          return True
        Left err -> do
          errorM tag ("[NG] " ++ inPath)
          mapM_ (errorM tag) (lines (show err))
          return False
    run path
      | takeExtension path == ".tjs" = runTask path "js" compileTJS
      | takeExtension path == ".ks" = runTask path "js" compileKAG
      | otherwise = do
          warningM tag ("Unknown file type: " ++ path)
          return True
--
outFileList :: FilePath -> FilePath -> FilePath -> IO [FilePath]
outFileList base to path = do
  let normpath = normalise path
  allItems <- getDirectoryContents path
  let items = (\f -> path ++ pathSeparator:f) <$> filter (\k -> k /= "." && k /= "..") allItems
  files <- filterM doesFileExist items
  justFolders <- filterM doesDirectoryExist items
  leftFiles <- mapM (outFileList base to) justFolders
  let allFiles = files ++ concat leftFiles
  let outDir = to </> makeRelative base path
  createDirectoryIfMissing True outDir
  B.writeFile (outDir </> "files-list.json") (JSON.encode (fmap (makeRelative path) allFiles))
  return allFiles

execute :: Opt -> IO ()
execute (Build from to) = build from to
