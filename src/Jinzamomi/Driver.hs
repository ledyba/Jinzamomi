module Jinzamomi.Driver (
  DriverOpt,
  execute,
  driverOpt
) where

import qualified Jinzamomi.Driver.Krkr as Krkr
import System.Log.Logger
import Options.Applicative

data DriverOpt =
    None
  | Krkr Krkr.Opt

tag :: String
tag = "Driver"

execute:: DriverOpt -> IO ()
execute opt@(Krkr args) = Krkr.execute args
execute None = errorM tag "Please specify driver"

driverOpt :: Parser DriverOpt
driverOpt = Krkr <$> hsubparser Krkr.opt
