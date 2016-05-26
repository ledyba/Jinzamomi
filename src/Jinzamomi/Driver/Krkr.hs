module Jinzamomi.Driver.Krkr (
  Opt,
  opt,
  execute
) where

import qualified Data.Text as T
import Language.TJS as TJS
import System.Log.Logger
import Options.Applicative
import Jinzamomi.Driver.Krkr.TJS2IR as TJS2IR
import Jinzamomi.Driver.IR as IR
import Jinzamomi.Driver.Util

data Opt =
    Build String String
  deriving (Show)

opt :: Mod CommandFields Opt
opt = command "krkr" (info buildCmd (progDesc "Krkr Driver."))
  where
    buildCmd = hsubparser $ command "build" (info buildOption (progDesc "Build krkr"))
    buildOption = Build <$> argument str (metavar "FROM") <*> argument str (metavar "TO")

compile :: FilePath -> FilePath -> IO ()
compile from to = do
  file <- readFile from
  case TJS.parse from (T.pack file) of
    Right ast -> do
      let out = T.unpack (IR.compile (TJS2IR.compile ast))
      writeFile to out
      infoM "Krkr" $ "Compiled source:\n" ++ out
    Left err -> error (show err)

build :: String -> String -> IO ()
build from to = do
  files <- enumAllFiles from
  mapM_ putStrLn files

execute :: Opt -> IO ()
execute (Build from to) = build from to
