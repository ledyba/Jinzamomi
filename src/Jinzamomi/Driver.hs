module Jinzamomi.Driver (
  executorOf
) where

import qualified Jinzamomi.Driver.Krkr as Krkr

executorOf :: String -> ([String] -> IO ())
executorOf "krkr" = Krkr.executor
executorOf s = error ("Unknown driver: " ++ s)
