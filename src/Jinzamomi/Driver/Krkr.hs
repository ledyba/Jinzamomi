module Jinzamomi.Driver.Krkr (
  Opt,
  opt,
  execute
) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified　Language.TJS as TJS
import System.Log.Logger
import Options.Applicative
import qualified Jinzamomi.Driver.Krkr.TJS2IR as TJS2IR
import qualified Jinzamomi.Driver.IR as IR
import Jinzamomi.Driver.Util
import System.FilePath (dropFileName, takeExtension, (</>))
import System.Directory (createDirectoryIfMissing)

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

compile :: FilePath -> FilePath -> FilePath -> IO Bool
compile from to src = do
  file <- TIO.readFile (from </> src)
  let outTo = to </> src
  createDirectoryIfMissing True (dropFileName outTo)
  case TJS.parse src file of
    Right ast -> do
      let out = IR.compile (TJS2IR.compile ast)
      infoM tag ("TJS2 Compile: " ++ src)
      TIO.writeFile outTo out
      return True
    Left err -> do
      errorM tag ("Error while compiling: " ++ src)
      mapM_ (errorM tag) (lines (show err))
      return False

build :: String -> String -> IO ()
build from to = do
  createDirectoryIfMissing True to
  files <- enumAllFiles from
  let tjsFiles = filter (\f -> takeExtension f == ".tjs") files
  results <- mapM (compile from to) tjsFiles
  let errors = length (filter not results)
  if errors /= 0 then
    errorM tag (concat ["compiling errors on ", show errors, "(/", show (length results), ") tjs2 files. done."])
  else
    return ()

execute :: Opt -> IO ()
execute (Build from to) = build from to
