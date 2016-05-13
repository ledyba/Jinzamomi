{-# LANGUAGE OverloadedStrings,FlexibleContexts #-}

module Uzume.Driver.IR (
  Node(..),
  compile
) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import Data.Text.Template

data Node =
    Block [Node]
  | If Node Node (Maybe Node)
  | While Node Node
  | For Node Node Node Node
  | Class [Text] [(Text, Node)]
  | Switch Node [(Node, [Node])] (Maybe [Node])
  | Function [Text] Node
  | Return Node
  | Continue
  | Break
  | Dot Node Text
  | This
  | Var Text
  | Declare [Text]
  | Let Node Node

nextIndent indent = T.concat ["  ", indent]

compile :: Node -> Text
compile node = compileBlock "" node

compileBlock :: Text -> Node -> Text
compileBlock indent node@(Block xs) = compile' indent node
compileBlock indent node = compile' indent node `T.append` ";"

compile' :: Text -> Node -> Text
--
compile' indent (Block nodes) =
  TL.toStrict $ substitute (T.unlines [
    "${indent}{",
    "${nodes}",
    "${indent}}"
  ]) ctx
  where
    ctx "indent" = indent
    ctx "nodes" = T.unlines (fmap ((`T.append` ";").compile' (nextIndent indent)) nodes)
--
compile' indent (Switch value nodes def) =
  TL.toStrict $ substitute (T.unlines [
    "${indent}switch(${value}){",
    "${nodes}",
    "${default}",
    "${indent}}"
  ]) ctx
  where
    ctx "indent" = indent
    ctx "value" = compile' "" value
    ctx "nodes" = T.unlines (fmap compileNode nodes)
    ctx "default" = case def of
      Just body ->
        T.unlines [
          T.concat [indent,"default:"],
          compileBody body
        ]
      Nothing ->
        T.concat [indent,"default:"]
    compileNode (value,body) =
        T.unlines [
          T.concat [indent,"case (",compile' "" value,"):"],
          compileBody body
        ]
    compileBody body = T.unlines (fmap ((`T.append` ";").compile' (nextIndent indent)) body)

--
compile' indent This = T.concat [indent, "this"]
compile' indent Continue = T.concat [indent, "continue"]
compile' indent Break = T.concat [indent, "break"]
--
compile' indent (While cond body) =
    TL.toStrict $ substitute (T.unlines [
      "while(${cond})",
      "${body}"
    ]) ctx
  where
    ctx "cond" = compile' indent cond
    ctx "body" = compileBlock indent body
--
compile' indent (If cond then_ else_) =
    TL.toStrict $ substitute (T.unlines [
      "if(${cond})",
      "${then}",
      "${else}"
    ]) ctx
  where
    ctx "cond" = compile' "" cond
    ctx "then" = compileBlock indent then_
    ctx "else" = case else_ of
      Just e -> T.unlines [T.concat [indent, "else"], compileBlock indent e]
      Nothing -> ""
--
compile' indent (For init_ cond_ up_ body) =
    TL.toStrict $ substitute (T.unlines [
      "for(${init}; ${cond}; ${up})",
      "${body}"
    ]) ctx
  where
    ctx "init" = compile' "" init_
    ctx "cond" = compile' "" cond_
    ctx "up" = compile' "" up_
    ctx "body" = compileBlock indent body
--
compile' indent (Function args body) =
    TL.toStrict $ substitute (T.unlines [
      "function(${args})",
      "${body}"
    ]) ctx
  where
    ctx "args" = T.intercalate ", " args
    ctx "body" = compileBlock indent body
--
compile' indent (Return value) =
    T.concat [indent,"return (",compile' "" value,")"]
--
compile' indent (Dot node1 attr) =
    T.concat [indent,compile' "" node1, ".", attr]
--
compile' indent (Var name) =
    T.concat [indent,name]
--
compile' indent (Declare names) =
    T.concat [indent, "var ", T.intercalate "," names]
--
compile' indent (Let node1 node2) =
    T.concat [indent, compile' "" node1, " = (", compile' "" node1,")"]
