{-# LANGUAGE OverloadedStrings #-}
module Jinzamomi.Driver.Krkr.KAG2IR (
  compile
) where

import Data.Maybe (fromMaybe)
import Language.KAG
import Language.TJS
import Data.Text (Text)
import qualified Jinzamomi.Driver.IR as IR
import qualified Jinzamomi.Driver.Krkr.TJS2IR as TJS2IR

compile :: [Kag] -> IR.Node
compile = fld' [] []
  where
    fld' labels nodes [] = IR.Dict [("labels", IR.Dict (reverse labels)), ("body", IR.Array (reverse nodes))]
    fld' labels nodes (x:xs) =
      case x of
        node@(KagLabel text _ _) -> fld' ((text, IR.Int (length nodes)):labels) (compileKag node:nodes) xs
        node -> fld' labels (compileKag node:nodes) xs

compileKag :: Kag -> IR.Node
compileLag (KagText text _) = IR.Str text
-- KagTag Text [(Text,Maybe KagValue)] SrcSpan
compileKag (KagTag func args _) = IR.Call (IR.Dot (IR.Raw "macros") func) [IR.Array (fmap toArg args)]
        where
          toArg (key, Just v) = IR.Array [IR.Str key, compileValue v]
          toArg (key, Nothing) = IR.Array [IR.Str key, IR.Undefined]
-- KagLabel Text (Maybe Text) SrcSpan
compileKag (KagNewline _) = IR.Dot (IR.Raw "macros") "__newline"
compileKag (KagLabel text desc _) = IR.Call (IR.Raw "macros.__label") [IR.Array [IR.Str text, IR.Str desc']]
  where
    desc' = fromMaybe "" desc

compileValue :: KagValue -> IR.Node
-- KagStrValue Text SrcSpan
compileValue (KagStrValue text _) = IR.Str text
-- KagTjsValue TJS.Expr SrcSpan
compileValue (KagTjsValue expr _) = TJS2IR.compileExpr expr
-- KagLabelValue Text SrcSpan
compileValue (KagLabelValue label _) = IR.Dot (IR.Raw "labels") label
