{-# LANGUAGE OverloadedStrings,FlexibleContexts #-}

module Jinzamomi.Driver.IR (
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
  | Switch Node [(Node, [Node])] (Maybe [Node])
  | Try Node Text Node
  | Function [Text] Node
  | Return Node
  | Continue
  | Break
  | Dot Node Text
  | Idx Node Node
  | This
  | Var Text
  | Declare [Text]
  | Assign Node Node
  | Call Node [Node]
  | New Node [Node]
  | Bin Node Text Node
  | Tri Node Node Node
  | PreUni Text Node
  | PostUni Node Text
  | Undefined
  | Null
  | Str Text
  | Array [Node]
  | Obj [(Node,Node)]
  | Nop
  | Raw Text
  | Int Int
  | Double Double
  | Bool Bool
  | Throw Node
  deriving (Ord,Eq,Show)

nextIndent indent = T.concat ["  ", indent]

jsEscape :: Text -> Text
jsEscape s = T.concat ["\"",esc s,"\""]
  where
    esc str = T.replace "\"" "\\\"" $
              T.replace "'" "\\'" $
              T.replace "\n" "\\n" $
              T.replace "\r" "\\r" $
              T.replace "\2029" "\\u2029" $
              T.replace "\\" "\\\\" str

compile :: Node -> Text
compile = compileBlock ""

compileBlock :: Text -> Node -> Text
compileBlock indent node@Block{} = compile' indent node
compileBlock indent node@If{} = compile' indent node
compileBlock indent node@While{} = compile' indent node
compileBlock indent node@Switch{} = compile' indent node
compileBlock indent node@Try{} = compile' indent node
compileBlock indent node@For{} = compile' indent node
compileBlock indent node = compile' indent node `T.append` ";"

join :: [Text] -> Text
join = T.intercalate "\n"

compile' :: Text -> Node -> Text
--
compile' indent (Block nodes) = join $ fmap (compileBlock (nextIndent indent)) $ filter (Nop /=) nodes
--
compile' indent (Try node@(Block _) var catch_@(Block _)) =
  TL.toStrict $ substitute (join [
    "${indent}try {",
    "${node}",
    "${indent}} catch(${var}) {",
    "${catch}",
    "${indent}}"
  ]) ctx
  where
    ctx "indent" = indent
    ctx "node" = compileBlock indent node
    ctx "var" = var
    ctx "catch" = compileBlock indent catch_

compile' indent (Try node var catch_) = compile' indent (Try (Block [node]) var (Block [catch_]))
--
compile' indent (Switch value nodes def) =
  TL.toStrict $ substitute (join [
    "${indent}switch(${value}){",
    "${nodes}",
    "${default}",
    "${indent}}"
  ]) ctx
  where
    ctx "indent" = indent
    ctx "value" = compile' "" value
    ctx "nodes" = join (fmap compileNode nodes)
    ctx "default" = case def of
      Just body ->
        join [
          T.concat [indent,"default:"],
          compileBody body
        ]
      Nothing ->
        T.concat [indent,"default:"]
    compileNode (value,body) =
        join [
          T.concat [indent,"case (",compile' "" value,"):"],
          compileBody body
        ]
    compileBody body = join (fmap ((`T.append` ";").compile' (nextIndent indent)) body)

--
compile' indent This = T.concat [indent, "this"]
compile' indent Continue = T.concat [indent, "continue"]
compile' indent Break = T.concat [indent, "break"]
--
compile' indent (While cond body) =
    TL.toStrict $ substitute (join [
      "${indent}while(${cond}) {",
      "${body}",
      "${indent}}"
    ]) ctx
  where
    ctx "indent" = indent
    ctx "cond" = compile' "" cond
    ctx "body" = compileBlock indent body
--
compile' indent (If cond then_ else_) =
    TL.toStrict $ substitute (join [
      "${indent}if(${cond}){",
      "${then}",
      "${else}"
    ]) ctx
  where
    ctx "indent" = indent
    ctx "cond" = compile' "" cond
    ctx "then" = compileBlock indent then_
    ctx "else" = case else_ of
      Just e -> join [T.concat [indent, "} else {"], compileBlock indent e, T.concat [indent, "}"]]
      Nothing -> indent `T.append` "}"
--
compile' indent (For init_ cond_ up_ body) =
    TL.toStrict $ substitute (join [
      "${indent}for(${init};${cond};${up}) {",
      "${body}",
      "${indent}}"
    ]) ctx
  where
    ctx "indent" = indent
    ctx "init" = compile' "" init_
    ctx "cond" = compile' "" cond_
    ctx "up" = compile' "" up_
    ctx "body" = compileBlock indent body
--
compile' indent (Function args body@(Block _)) =
    TL.toStrict $ substitute (join [
      "${indent}function(${args}) {",
      "${indent}${body}",
      "${indent}}"
    ]) ctx
  where
    ctx "indent" = indent
    ctx "args" = T.intercalate ", " args
    ctx "body" = compileBlock indent body
compile' indent (Function args node) = compile' indent (Function args (Block [node]))
--
compile' indent Undefined =
    T.concat [indent,"(undefined)"]
--
compile' indent Nop = ""
--
compile' indent Null =
    T.concat [indent,"(null)"]
--
compile' indent (Bool True) =
    T.concat [indent,"(true)"]
compile' indent (Bool False) =
    T.concat [indent,"(false)"]
--
compile' indent (Str str) =
    T.concat [indent, jsEscape str]
--
compile' indent (Int n) =
    T.concat [indent, T.pack (show n)]
--
compile' indent (Double r) =
    T.concat [indent, T.pack (show r)]
--
compile' indent (Raw str) =
    T.concat [indent, str]
--
compile' indent (Array nodes) =
    T.concat [indent,"[", T.intercalate ", " $ fmap (compile' "") nodes, "]"]
--
compile' indent (Return value) =
    T.concat [indent,"return (",compile' "" value,")"]
--
compile' indent (Dot node attr) =
    T.concat [indent,compile' "" node, "[", jsEscape attr ,"]"]
--
compile' indent (Idx node attr) =
    T.concat [indent,compile' "" node, "[", compile' "" attr, "]"]
--
compile' indent (Var name) =
    T.concat [indent,name]
--
compile' indent (Declare names) =
    T.concat [indent, "var ", T.intercalate "," names]
--
compile' indent (Assign node1 node2) =
    T.concat [indent, compile' "" node1, " = (", compile' "" node2,")"]
--
compile' indent (Bin node1 op node2) =
    T.concat [indent, compile' "" node1, " ", op, " ", compile' "" node2]
--
compile' indent (Tri cond t_ f_) =
    T.concat [indent, "(", compile' "" cond, ") ? (", compile' "" t_, ") : (", compile' "" f_, ")"]
--
compile' indent (PreUni op node) =
    T.concat [indent, op, "(", compile' "" node,")"]
--
compile' indent (PostUni node op) =
    T.concat [indent, "(", compile' "" node,")",op]
--
compile' indent (New constructor args) =
    T.concat [indent, "new (", compile' "" constructor, ")(",T.intercalate ", " (fmap (compile' "") args),")"]
--
compile' indent (Call target args) =
    T.concat [indent, "(", compile' "" target, ")(",T.intercalate ", " (fmap (compile' "") args),")"]
--
compile' indent (Throw node) =
    T.concat [indent, "throw (", compile' "" node,")"]
-- compile' _ node = error $ "Please implement compile' for " ++ show node
