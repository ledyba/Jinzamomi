{-# LANGUAGE OverloadedStrings #-}
module Jinzamomi.Driver.Krkr.KAG2IR (
  compile
) where

import Language.KAG
import Language.TJS
import Data.Text (Text)
import qualified Jinzamomi.Driver.IR as IR

compile :: [Kag] -> IR.Node
compile kags = IR.Array (fmap compileKag kags)

compileKag :: Kag -> IR.Node
compileLag (KagText text _) = IR.Nop
-- KagTag Text [(Text,Maybe KagValue)] SrcSpan
compileKag (KagTag text args _) = IR.Nop
-- KagLabel Text (Maybe Text) SrcSpan
compileKag (KagLabel text desc _) = IR.Nop
compileKag (KagNewline _) = IR.Nop

compileValue :: KagValue -> IR.Node
-- KagStrValue Text SrcSpan
compileValue (KagStrValue text _) = IR.Nop
-- KagTjsValue TJS.Expr SrcSpan
compileValue (KagTjsValue expr _) = IR.Nop
-- KagLabelValue Text SrcSpan
compileValue (KagLabelValue label _) = IR.Nop
