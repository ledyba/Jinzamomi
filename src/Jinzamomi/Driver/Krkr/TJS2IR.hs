{-# LANGUAGE OverloadedStrings #-}
module Jinzamomi.Driver.Krkr.TJS2IR (
  compileStmt,
  compileExpr
) where

import Language.KAG
import Language.TJS
import Control.Monad.State
import qualified Data.Text as T
import qualified Jinzamomi.Driver.IR as IR
import Data.Maybe (maybeToList)

type Compile = State Env
data Env = Env {
  localCount :: Int,
  withCount :: Int,
  tempCount :: Int
}
data Scope = Scope {
  localObj :: T.Text,
  withObj :: T.Text
}

initEnv :: Env
initEnv = Env {
  localCount=0,
  withCount=0,
  tempCount=0
}

initScope :: Scope
initScope = Scope {
  localObj = "scope",
  withObj = "global"
}

global :: IR.Node
global = IR.Raw "global"

local :: Scope -> T.Text -> IR.Node
local scope = IR.Dot (IR.Var (localObj scope))

withNewScope :: Scope -> (Scope -> Compile [IR.Node]) -> Compile IR.Node
withNewScope scope f = do
  env <- get
  let nextLocal = "scope" `T.append` T.pack (show (localCount env))
  put $ env { localCount = 1 + localCount env }
  let scope' = scope { localObj = nextLocal }
  let currentLocal = localObj scope
  stmts' <- f scope'
  return $ IR.Block $ concat [
      [IR.Declare [nextLocal]],
      [IR.Assign (IR.Var nextLocal) (IR.Call (IR.Dot (IR.Var "Object") "create") [IR.Var currentLocal])],
      stmts'
    ]

with :: Scope -> IR.Node -> (Scope -> Compile IR.Node) -> Compile IR.Node
with scope obj' f = do
  env <- get
  let nextWithObj = "with" `T.append` T.pack (show (localCount env))
  put $ env { withCount = 1 + withCount env }
  let scope' = scope { withObj = nextWithObj }
  node' <- f scope'
  return $ IR.Block [
      IR.Declare [nextWithObj],
      IR.Assign (IR.Var nextWithObj) obj',
      node'
    ]

withTemp :: (IR.Node -> Compile [IR.Node]) -> Compile IR.Node
withTemp f = do
  env <- get
  let nextTempObj = "temp" `T.append` T.pack (show (tempCount env))
  put $ env { tempCount = 1 + tempCount env }
  body <- f (IR.Var nextTempObj)
  return (IR.Block $ IR.Declare [nextTempObj] : body)

createObject :: IR.Node
createObject = IR.Call (IR.Dot (IR.Var "Object") "create") [IR.Null]

compileStmt :: Stmt -> IR.Node
compileStmt stmt =
    let body = IR.Function ["global", "scope"] (evalState (compileStmt' initScope stmt) initEnv)
    in
      IR.Call body [IR.Raw "jinzamomi.krkr.global", IR.Raw "jinzamomi.krkr.global"]

compileExpr :: Expr -> IR.Node
compileExpr expr =
    let body = IR.Function ["global", "scope"] (IR.Block [IR.Return (evalState (compileExpr' initScope expr) initEnv)])
    in
      IR.Call body [IR.Raw "jinzamomi.krkr.global", IR.Raw "jinzamomi.krkr.global"]

compileStmt' :: Scope -> Stmt -> Compile IR.Node
--If       Expr Stmt (Maybe Stmt) SrcSpan
compileStmt' scope (If expr then_ else_ _) = do
  e <- compileExpr' scope expr
  th <- compileStmt' scope then_
  els <- mapM (compileStmt' scope) else_
  return $ IR.If e th els
--Switch   Expr [(Expr, [Stmt])] (Maybe [Stmt]) SrcSpan
compileStmt' scope (Switch value bodies def _) = do
  value' <- compileExpr' scope value
  bodies' <- mapM compileConds bodies
  def' <- mapM (mapM (compileStmt' scope)) def
  return $ IR.Switch value' bodies' def'
  where
    compileConds (expr, stmts) = do
      expr' <- compileExpr' scope expr
      stmts' <- mapM (compileStmt' scope) stmts
      return (expr', stmts')
--While    Expr Stmt SrcSpan
compileStmt' scope (While cond body _) = do
  cond' <- compileExpr' scope cond
  body' <- compileStmt' scope body
  return $ IR.While cond' body'
--Break    SrcSpan
compileStmt' scope (Break _) = return IR.Break
--With     Expr Stmt SrcSpan
compileStmt' scope (With obj stmt _) = do
  obj' <- compileExpr' scope obj
  with scope obj' $ \scope' -> compileStmt' scope' stmt
--Try      Stmt Identifer Stmt SrcSpan
compileStmt' scope (Try try (Identifer err) catch_ _) = do
  try' <- withNewScope scope $ \scope' -> compileStmt' scope' try >>= \r -> return [r]
  catch' <- withNewScope scope $ \scope' -> compileStmt' scope' catch_ >>= \r -> return [r]
  return (IR.Try try' err (IR.Block [
    IR.Assign (local scope err) (IR.Var err),
    catch'
    ]))
--Throw    Expr SrcSpan
compileStmt' scope (Throw expr _) = do
  expr' <- compileExpr' scope expr
  return (IR.Throw expr')
--For      Stmt Expr Expr Stmt SrcSpan
compileStmt' scope (For init_ cond_ next_ body_ _) =
  withNewScope scope $ \scope' -> do
    init' <- mapM (compileStmt' scope') init_
    cond' <- mapM (compileExpr' scope') cond_
    next' <- mapM (compileExpr' scope') next_
    body' <- compileStmt' scope' body_
    return [maybeToIR init', IR.For IR.Nop (maybeToIR cond') (maybeToIR next') body']
  where
    maybeToIR Nothing = IR.Nop
    maybeToIR (Just node) = node
--Continue SrcSpan
compileStmt' scope (Continue _) = return IR.Continue
--Return   Expr SrcSpan
compileStmt' scope (Return expr _) = do
  expr' <- compileExpr' scope expr
  return $ IR.Return expr'
--Prop     Identifer (Maybe Stmt) (Maybe (Identifer, Stmt)) SrcSpan
compileStmt' scope (Prop (Identifer name) getter setter _) =
  withTemp $ \tmp -> do
    let IR.Var tempName = tmp
    getter' <- mapM compileGetter getter
    setter' <- mapM compileSetter setter
    return $ concat [
        [IR.Assign tmp createObject],
        case getter' of
          Just x -> [IR.Assign (IR.Dot tmp "get") x]
          Nothing -> [],
        case setter' of
          Just x -> [IR.Assign (IR.Dot tmp "set") x]
          Nothing -> [],
        [IR.Call (IR.Dot (IR.Var "Object") "defineProperty") [IR.Var (localObj scope), IR.Str name, tmp]]
      ]
  where
    compileGetter stmt = do
      stmt' <- compileStmt' scope stmt
      return (IR.Function [] stmt')
    compileSetter (Identifer name, stmt) = do
      stmt' <- compileStmt' scope stmt
      return (IR.Function [name] stmt')

--Class    Identifer (Maybe [Identifer]) [Stmt] SrcSpan
-- TODO: 継承
compileStmt' scope (Class (Identifer name) exts stmts _) = do
  fbody' <- withNewScope scope $ \scope' -> do
    body' <- mapM (compileStmt' scope') stmts
    return (body' ++ [IR.Return (IR.Var (localObj scope'))])
  return (IR.Assign (IR.Dot global name) (IR.Call (IR.Function [] fbody') []))
--Func     Identifer [FuncArg] Stmt SrcSpan
compileStmt' scope (Func (Identifer name) args stmt _) = do
  body' <- withNewScope scope $ \scope' -> do
    stmt' <- compileStmt' scope' stmt
    return [stmt']
  (args', nodes') <- compileFuncArg scope args
  return $ IR.Assign (local scope name) (IR.Function args' (IR.Block $ nodes' ++ [body']))

--Block    [Stmt] SrcSpan
compileStmt' scope (Block [] _) = return (IR.Block [])
compileStmt' scope (Block stmts _) =
  withNewScope scope $ \scope' -> mapM (compileStmt' scope') stmts
--Var      [(Identifer,Maybe Expr)] SrcSpan
compileStmt' scope (Var assigns _) = do
    vars <- mapM compileVar assigns
    return (IR.Block vars)
  where
    compileVar (Identifer name, Nothing) = return $ IR.Assign (local scope name) IR.Undefined
    compileVar (Identifer name, Just expr) = do
      expr' <- compileExpr' scope expr
      return $ IR.Assign (local scope name) expr'
--Exec     Expr SrcSpan
compileStmt' scope (Exec expr _) = compileExpr' scope expr
--Nop      SrcSpan
compileStmt' scope (Nop _) = return IR.Nop

compileExpr' :: Scope -> Expr -> Compile IR.Node
--Bin      String Expr Expr SrcSpan
compileExpr' scope (Bin "=" (PreUni "&" (Dot e1 (Identifer prop) _) _) e2 _) = do
  e1' <- compileExpr' scope e1
  e2' <- compileExpr' scope e2
  return (IR.Call (IR.Dot (IR.Var "Object") "defineProperty") [e1', IR.Str prop, e2'])
compileExpr' scope (Bin "\\" e1 e2 _) = do
  e1' <- compileExpr' scope e1
  e2' <- compileExpr' scope e2
  return (IR.Call (IR.Raw "Math.floor") [IR.Bin e1' "/" e2'])
compileExpr' scope (Bin "if" v cond _) = do
  v' <- compileExpr' scope v
  cond' <- compileExpr' scope cond
  return (IR.Tri cond' v' IR.Null)
compileExpr' scope (Bin "instanceof" v cls _) = do
  v' <- compileExpr' scope v
  cls' <- compileExpr' scope cls
  return (IR.Call (IR.Dot global "__instanceof") [v', cls'])
compileExpr' scope (Bin "incontextof" v cls _) = do
  v' <- compileExpr' scope v
  self' <- compileExpr' scope cls
  return (IR.Call (IR.Dot v' "bind") [v', self'])
--
compileExpr' scope (Bin op e1 e2 _) = do
  e1' <- compileExpr' scope e1
  e2' <- compileExpr' scope e2
  return $ IR.Bin e1' (T.pack op) e2'
--PreUni   String Expr SrcSpan
compileExpr' scope (PreUni "&" (Dot e (Identifer prop) _) _) = do
  e' <- compileExpr' scope e
  return (IR.Call (IR.Raw "uzume.krkr.getPropertyDescriptor") [e', IR.Str prop])
compileExpr' scope (PreUni "&" (Ident (Identifer prop) _) _) =
  return (IR.Call (IR.Var "uzume.krkr.getPropertyDescriptor") [IR.Var (localObj scope), IR.Str prop])
compileExpr' scope (PreUni "#" e _) = do
  e' <- compileExpr' scope e
  return (IR.Call (IR.Dot global "__ord") [e'])
compileExpr' scope (PreUni "$" e _) = do
  e' <- compileExpr' scope e
  return (IR.Call (IR.Dot global "__chr") [e'])
compileExpr' scope (PreUni op e _) = do
  e' <- compileExpr' scope e
  return $ IR.PreUni (T.pack op) e'
--PostUni  Expr String SrcSpan
compileExpr' scope (PostUni e "isvalid" _) = do
  e' <- compileExpr' scope e
  return $ IR.Call (IR.Dot global "__isvalid") [e']
compileExpr' scope (PostUni e op _) = do
  e' <- compileExpr' scope e
  return $ IR.PostUni e' (T.pack op)
--Tri      Expr Expr Expr SrcSpan
compileExpr' scope (Tri cond a b _) = do
  cond' <- compileExpr' scope cond
  a' <- compileExpr' scope a
  b' <- compileExpr' scope b
  return $ IR.Tri cond' a' b'
--Cast     Identifer Expr SrcSpan
compileExpr' scope (Cast (Identifer name) val _) = do
  val' <- compileExpr' scope val
  return $ IR.Call (IR.Raw "global.__cast") [IR.Str name, val']
--Int      Integer SrcSpan
compileExpr' scope (Int i _) = return $ IR.Int (fromIntegral i)
--Real     Double SrcSpan
compileExpr' scope (Real d _) = return $ IR.Double d
--Str      Text SrcSpan
compileExpr' scope (Str text _) = return $ IR.Str text
--Ident    Identifer SrcSpan
compileExpr' scope (Ident (Identifer name) _) = return (local scope name)
--Array    [Expr] SrcSpan
compileExpr' scope (Array exprs _) = do
    exprs' <- mapM (compileExpr' scope) exprs
    return $ IR.Array exprs'
--Dict     [(Expr, Expr)] SrcSpan
compileExpr' scope (Dict kvs _) = do
    kvs' <- mapM compile' kvs
    body' <- withTemp $ \tmp ->
      return (concat [
            [IR.Assign tmp createObject],
            [IR.Assign (IR.Idx tmp k) v | (k,v) <- kvs'],
            [IR.Return tmp]
        ])
    return $ IR.Call (IR.Function [] body') []
  where
    compile' (k,v) = do
      k' <- compileExpr' scope k
      v' <- compileExpr' scope v
      return (k',v')
--AnonFunc [FuncArg] Stmt SrcSpan
compileExpr' scope (AnonFunc args stmt _) = do
  body' <- withNewScope scope $ \scope' -> fmap return (compileStmt' scope' stmt)
  (args', nodes') <- compileFuncArg scope args
  return $ IR.Function args' body'
--Index    Expr Expr SrcSpan
compileExpr' scope (Index expr attr _) = do
  expr' <- compileExpr' scope expr
  attr' <- compileExpr' scope attr
  return (IR.Idx expr' attr')
--Call     Expr [ApplyArg] SrcSpan
compileExpr' scope (Call expr args _) = do
  expr' <- compileExpr' scope expr
  compileFuncApply scope expr' args
--Dot      Expr Identifer SrcSpan
compileExpr' scope (Dot expr (Identifer name) _) = do
  expr' <- compileExpr' scope expr
  return (IR.Dot expr' name)
--Null     SrcSpan
compileExpr' scope (Null _) = return IR.Null
--WithThis SrcSpan
compileExpr' scope (WithThis _) = return (IR.Var (withObj scope))
--Regexp String String SrcSpan TODO: flag
compileExpr' scope (Regexp pat flags _) = return (IR.New (IR.Raw "RegExp") [IR.Str pat, IR.Str (T.pack flags)])

compileFuncApply :: Scope -> IR.Node -> [ApplyArg] -> Compile IR.Node
compileFuncApply scope target' args = do
    args' <- mapM compile args
    let apply = or (fmap snd args')
    if apply
      then case target' of
          IR.Dot self' func' ->
            return (IR.Call (IR.Function ["self", "args"] (IR.Block [
                IR.Call (IR.Dot (IR.Dot (IR.Var "self") func') "apply") [IR.Var "self", IR.Var "args"]
                ])) [self', IR.Call (IR.Dot (IR.Var "Array") "concat") (fmap toConcat args')])
          _ -> return (IR.Call (IR.Dot target' "call") (IR.Null:fmap fst args'))
    else return (IR.Call target' (fmap fst args'))
  where
    toConcat (expr, True) = expr
    toConcat (expr, False) = IR.Array [expr]
    compile ApplyLeft = return (IR.Var "arguments", True)
    compile (ApplyArray expr) = do
      expr' <- compileExpr' scope expr
      return (expr', True)
    compile ApplyVoid = return (IR.Null, False)
    compile (ApplyExpr expr) = do
      expr' <- compileExpr' scope expr
      return (expr', False)

compileFuncArg :: Scope -> [FuncArg] -> Compile ([T.Text], [IR.Node])
compileFuncArg scope args = do
    args' <- compileFuncArg' args ([], [])
    return (done args')
  where
    done (args, nodes) = (reverse args, concat $ reverse nodes)
    local = localObj scope
    compileFuncArg' :: [FuncArg] -> ([T.Text], [[IR.Node]]) -> Compile ([T.Text], [[IR.Node]])
    compileFuncArg' _args ctx@(args, nodes) =
        case _args of
          [FuncLeft] -> return ctx
          [FuncArray (Identifer name)] -> return (name:args, [
            IR.Assign (IR.Dot (IR.Var local) name) (IR.Call (IR.Dot (IR.Var "argument") "slice") [IR.Int (length args)])
            ]:nodes)
          FuncArg (Identifer name) expr:xs ->
            case expr of
              Just expr' -> do
                expr'' <- compileExpr' scope expr'
                return (name:args, [IR.Assign (IR.Dot (IR.Var local) name) (IR.Bin (IR.Var name) "||" expr'')]:nodes)
              Nothing -> return (name:args, [IR.Assign (IR.Dot (IR.Var local) name) (IR.Var name)]:nodes)
          [] -> return ([], [])
          _ -> error ("Error while compiling function arguments: " ++ show _args)
