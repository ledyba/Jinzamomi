module Main where

import qualified Data.Text as T
import Language.TJS as TJS
import Uzume.Driver.Krkr.TJS2IR as TJS2IR
import Uzume.Driver.IR as IR

main :: IO ()
main = do
  file <- readFile "ext/kag3/data/startup.tjs"
  case TJS.parse "startup.tjs" (T.pack file) of
    Right ast -> do
      let out = T.unpack (IR.compile (TJS2IR.compile ast))
      writeFile "runtime/test/proj/startup.js" out
      putStrLn out
    Left err -> error (show err)
