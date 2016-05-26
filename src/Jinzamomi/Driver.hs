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
  | Krkr [String]

tag :: String
tag = "Driver"

execute:: DriverOpt -> IO ()
execute opt@(Krkr args) = Krkr.execute args
execute None = errorM tag "Please specify driver"

krkrOpt :: Mod CommandFields DriverOpt
krkrOpt = command "krkr" (info krkrOption (progDesc "Krkr Driver."))
  where
    krkrOption = Krkr <$> many (argument str (metavar "TARGET..."))

driverOpt = hsubparser krkrOpt
