module Main where

import qualified Jinzamomi.Driver as D
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import qualified GHC.IO.Handle.FD as FD
import System.Environment (getArgs)
import Options.Applicative

data Opt = Opt {
  loglevel :: String,
  driverOpt :: D.DriverOpt
}

commonOpt :: Parser (D.DriverOpt -> Opt)
commonOpt = Opt
       <$> strOption
           ( long "log"
          <> short 'l'
          <> value "debug"
          <> showDefault
          <> metavar "LOGLEVEL"
          <> help "Log Level(debug/info/notice/warning/error/critical/alert/emergency)." )

versionOpt :: Parser (a -> a)
versionOpt = infoOption "Jinzamomi 0.0.0"
  (  long "version"
  <> short 'v'
  <> help "Print version information" )

loglevelOf :: String -> Priority
loglevelOf "debug" = DEBUG
loglevelOf "info" = INFO
loglevelOf "notice" = NOTICE
loglevelOf "warning" = WARNING
loglevelOf "error" = ERROR
loglevelOf "critical" = CRITICAL
loglevelOf "alert" = ALERT
loglevelOf "emergency" = EMERGENCY
loglevelOf _ = DEBUG

setupLogger :: Priority -> IO ()
setupLogger level = do
  logger <- getRootLogger
  handler <-  verboseStreamHandler FD.stdout DEBUG
  let handler' = setFormatter handler (tfLogFormatter "%y/%m/%d %m:%d:%y" "[$time $loggername $prio] $msg")
  saveGlobalLogger $ setLevel level $ addHandler handler' $ removeHandler logger

tag :: String
tag = "Main"

main :: IO ()
main = do
    opt <- execParser opts
    let loglev = loglevelOf (loglevel opt)
    setupLogger loglev
    noticeM tag " *** Jinzamomi *** "
    noticeM tag ("Log Level: " ++ show loglev)
    D.execute (driverOpt opt)
    --drv:args <- getArgs
    --let executor = D.executorOf drv
    --executor args
    --Main.compile "ext/kag3/data/startup.tjs" "runtime/test/proj/startup.tjs.js"
    --Main.compile "ext/kag3/data/system/Initialize.tjs" "runtime/test/proj/system/Initialize.tjs.js"
  where
    cmds = D.driverOpt
    opts = info (versionOpt <*> helper <*> commonOpt <*> cmds)
      ( fullDesc
     <> progDesc "Ahead of Time transpiler from Bishoujo-games to HTML5 "
     <> header "Jinzamomi" )
