{-# LANGUAGE OverloadedStrings #-}
module Uzume.Driver.Krkr.TJS2IR (
  compile
) where

import Language.KAG
import Language.TJS
import Control.Monad.State
import qualified Data.Text as T
import qualified Uzume.Driver.IR as IR

type Compile = State Env
data Env = Env {
  localCount :: Int,
  withCount :: Int
}
data Scope = Scope {
  localObj :: T.Text,
  withObj :: T.Text
}

initEnv :: Env
initEnv = Env {
  localCount=0,
  withCount=0
}

initScope :: Scope
initScope = Scope {
  localObj = "local",
  withObj = "global"
}

newScope :: Scope -> Compile Scope
newScope scope = do
  env <- get
  let nextLocal = "local" `T.append` T.pack (show (localCount env))
  put $ env { localCount = 1 + localCount env }
  return $ scope {
    localObj = nextLocal
  }

with :: Scope -> Compile Scope
with scope = do
  env <- get
  let nextWithObj = "with" `T.append` T.pack (show (localCount env))
  put $ env { withCount = 1 + withCount env }
  return $ scope {
    withObj = nextWithObj
  }

compile :: Stmt -> IR.Node
compile stmt = IR.Function ["global", "local"] (evalState (compileStmt initScope stmt) initEnv)

compileStmt :: Scope -> Stmt -> Compile IR.Node
--If       Expr Stmt (Maybe Stmt) SrcSpan
compileStmt scope (If expr then_ else_ _) = do
  e <- compileExpr scope expr
  th <- compileStmt scope then_
  els <- mapM (compileStmt scope) else_
  return $ IR.If e th els
--Switch   Expr [(Expr, [Stmt])] (Maybe [Stmt]) SrcSpan
compileStmt scope (Switch value bodies def _) = do
  value' <- compileExpr scope value
  bodies' <- mapM compileConds bodies
  def' <- mapM (mapM (compileStmt scope)) def
  return $ IR.Switch value' bodies' def'
  where
    compileConds (expr, stmts) = do
      expr' <- compileExpr scope expr
      stmts' <- mapM (compileStmt scope) stmts
      return (expr', stmts')
--While    Expr Stmt SrcSpan
compileStmt scope (While cond body _) = do
  cond' <- compileExpr scope cond
  body' <- compileStmt scope body
  return $ IR.While cond' body'
--Break    SrcSpan
compileStmt scope (Break _) = return IR.Break
--With     Expr Stmt SrcSpan
compileStmt scope (With obj stmt _) = do
  obj' <- compileExpr scope obj
  scope' <- with scope
  let nextWithObj = withObj scope'
  stmt' <- compileStmt scope' stmt
  return $ IR.Block [
      IR.Declare [nextWithObj],
      IR.Let (IR.Var nextWithObj) obj',
      stmt'
    ]
--Try      Stmt Identifer Stmt SrcSpan
--Throw    Expr SrcSpan
--For      Stmt Expr Expr Stmt SrcSpan
--Continue SrcSpan
compileStmt scope (Continue _) = return IR.Continue
--Return   Expr SrcSpan
compileStmt scope (Return expr _) = do
  expr' <- compileExpr scope expr
  return $ IR.Return expr'
--Prop     Identifer (Maybe Stmt) (Maybe (Identifer, Stmt)) SrcSpan
--FIXME Use "local[varname]"
compileStmt scope (Prop (Identifer name) getter setter _) = return (IR.Block [])
--Class    Identifer (Maybe [Identifer]) [Stmt] SrcSpan
--Func     Identifer [FuncArg] Stmt SrcSpan
--Block    [Stmt] SrcSpan
compileStmt scope (Block stmts _) = do
  let currentLocal = localObj scope
  scope' <- newScope scope
  stmts' <- mapM (compileStmt scope') stmts
  let nextLocal = localObj scope'
  return $ IR.Block $ concat [
      [IR.Declare [nextLocal]],
      [IR.Let (IR.Var nextLocal) (IR.Call (IR.Dot (IR.Var "Object") "create") [IR.Var currentLocal])],
      stmts'
    ]
--Var      [(Identifer,Maybe Expr)] SrcSpan
--Exec     Expr SrcSpan
compileStmt scope (Exec expr _) = compileExpr scope expr
--Nop      SrcSpan
compileStmt _ stmt = error ("Please implement compileStmt for: "++show stmt)

compileExpr :: Scope -> Expr -> Compile IR.Node
--Bin      String Expr Expr SrcSpan
compileExpr scope (Bin op e1 e2 _) = do
  e1' <- compileExpr scope e1
  e2' <- compileExpr scope e2
  return $ IR.Bin e1' (T.pack op) e2'
--PreUni   String Expr SrcSpan
compileExpr scope (PreUni op e _) = do
  e' <- compileExpr scope e
  return $ IR.PreUni (T.pack op) e'
--PostUni  Expr String SrcSpan
compileExpr scope (PostUni e op _) = do
  e' <- compileExpr scope e
  return $ IR.PostUni e' (T.pack op)
--Tri      Expr Expr Expr SrcSpan
--Cast     Identifer Expr SrcSpan
--Int      Integer SrcSpan
--Real     Double SrcSpan
--Str      Text SrcSpan
compileExpr scope (Str text _) = return $ IR.Str text
--Ident    Identifer SrcSpan
compileExpr scope (Ident (Identifer name) _) = return $ IR.Var name
--Array    [Expr] SrcSpan
--Dict     [(Expr, Expr)] SrcSpan
--AnonFunc [FuncArg] Stmt SrcSpan
--Index    Expr Expr SrcSpan
--Call     Expr [ApplyArg] SrcSpan
compileExpr scope (Call expr args _) = do
  expr' <- compileExpr scope expr
  args' <- mapM (compileApplyArg scope) args
  return $ IR.Call expr' args'
--Dot      Expr Identifer SrcSpan
compileExpr scope (Dot expr (Identifer name) _) = do
  expr' <- compileExpr scope expr
  return (IR.Dot expr' name)
--Null     SrcSpan
compileExpr scope (Null _) = return IR.Null
--WithThis SrcSpan
compileExpr scope (WithThis _) = return (IR.Var (withObj scope))
compileExpr _ expr = error ("Please implement compileExpr for: "++show expr)

compileApplyArg :: Scope -> ApplyArg -> Compile IR.Node
--ApplyLeft
-- FIXME
compileApplyArg _ ApplyLeft = return $ IR.Var "arguments"
--ApplyArray Expr
compileApplyArg scope (ApplyArray expr) = compileExpr scope expr
--ApplyExpr Expr
compileApplyArg scope (ApplyExpr expr) = compileExpr scope expr
--ApplyVoid
compileApplyArg scope ApplyVoid = return IR.Undefined

compileFuncArg :: FuncArg -> IR.Node
compileFuncArg = undefined
--data FuncArg =
--FuncLeft
--FuncArray Identifer
--FuncArg Identifer (Maybe Expr)
