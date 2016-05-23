module Jinzamomi.Driver.Krkr (
  executor
) where

import qualified Data.Text as T
import Language.TJS as TJS
import System.Log.Logger
import Jinzamomi.Driver.Krkr.TJS2IR as TJS2IR
import Jinzamomi.Driver.IR as IR

compile :: FilePath -> FilePath -> IO ()
compile from to = do
  file <- readFile from
  case TJS.parse from (T.pack file) of
    Right ast -> do
      let out = T.unpack (IR.compile (TJS2IR.compile ast))
      writeFile to out
      infoM "Krkr" $ "Compiled source:\n" ++ out
    Left err -> error (show err)

build :: [String] -> IO ()
build xs = return ()

executor :: [String] -> IO ()
executor ("build":xs) = build xs
executor xs = error ("Unknown command" ++ show xs)