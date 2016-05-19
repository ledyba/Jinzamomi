module Main where

import qualified Data.Text as T
import Language.TJS as TJS
import Jinzamomi.Driver.Krkr.TJS2IR as TJS2IR
import Jinzamomi.Driver.IR as IR
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import GHC.IO.Handle.FD as FD

compile :: FilePath -> FilePath -> IO ()
compile from to = do
  file <- readFile from
  case TJS.parse from (T.pack file) of
    Right ast -> do
      let out = T.unpack (IR.compile (TJS2IR.compile ast))
      writeFile to out
      infoM "Krkr" $ "Compiled source:\n" ++ out
    Left err -> error (show err)

setupLogger :: Priority -> IO ()
setupLogger level = do
  logger <- getRootLogger
  handler <-  verboseStreamHandler FD.stdout DEBUG
  let handler' = setFormatter handler (simpleLogFormatter "[$time][$prio@$loggername] $msg")
  saveGlobalLogger $ setLevel level $ addHandler handler' $ removeHandler logger

main :: IO ()
main = do
  setupLogger INFO
  Main.compile "ext/kag3/data/startup.tjs" "runtime/test/proj/startup.js"
  Main.compile "ext/kag3/data/system/Initialize.tjs" "runtime/test/proj/system/Initialize.js"
