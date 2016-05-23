module Main where

import qualified Jinzamomi.Driver as D
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import qualified GHC.IO.Handle.FD as FD
import System.Environment (getArgs)

setupLogger :: Priority -> IO ()
setupLogger level = do
  logger <- getRootLogger
  handler <-  verboseStreamHandler FD.stdout DEBUG
  let handler' = setFormatter handler (simpleLogFormatter "[$time][$prio@$loggername] $msg")
  saveGlobalLogger $ setLevel level $ addHandler handler' $ removeHandler logger

main :: IO ()
main = do
  setupLogger INFO
  drv:args <- getArgs
  let executor = D.executorOf drv
  executor args
  --Main.compile "ext/kag3/data/startup.tjs" "runtime/test/proj/startup.tjs.js"
  --Main.compile "ext/kag3/data/system/Initialize.tjs" "runtime/test/proj/system/Initialize.tjs.js"
