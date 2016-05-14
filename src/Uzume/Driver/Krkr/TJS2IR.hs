module Uzume.Driver.Krkr.TJS2IR (
  compile
) where

import Language.KAG
import Language.TJS
import Control.Monad.State
import qualified Data.Text as T
import qualified Uzume.Driver.IR as IR

type Compile = State Env
data Env = Env

compile :: Stmt -> IR.Node
compile stmt = evalState (compileStmt stmt) Env{}

compileStmt :: Stmt -> Compile IR.Node
--If       Expr Stmt (Maybe Stmt) SrcSpan
compileStmt (If expr then_ else_ _) = do
  e <- compileExpr expr
  th <- compileStmt then_
  els <- mapM compileStmt else_
  return $ IR.If e th els
--Switch   Expr [(Expr, [Stmt])] (Maybe [Stmt]) SrcSpan
compileStmt (Switch value bodies def _) = do
  value' <- compileExpr value
  bodies' <- mapM compileConds bodies
  def' <- mapM (mapM compileStmt) def
  return $ IR.Switch value' bodies' def'
  where
    compileConds (expr, stmts) = do
      expr' <- compileExpr expr
      stmts' <- mapM compileStmt stmts
      return (expr', stmts')
--While    Expr Stmt SrcSpan
compileStmt (While cond body _) = do
  cond' <- compileExpr cond
  body' <- compileStmt body
  return $ IR.While cond' body'
--Break    SrcSpan
compileStmt (Break _) = return IR.Break
--With     Expr Stmt SrcSpan
--Try      Stmt Identifer Stmt SrcSpan
--Throw    Expr SrcSpan
--For      Stmt Expr Expr Stmt SrcSpan
--Continue SrcSpan
compileStmt (Continue _) = return IR.Continue
--Return   Expr SrcSpan
compileStmt (Return expr _) = do
  expr' <- compileExpr expr
  return $ IR.Return expr'
--Prop     Identifer (Maybe Stmt) (Maybe (Identifer, Stmt)) SrcSpan
--Class    Identifer (Maybe [Identifer]) [Stmt] SrcSpan
--Func     Identifer [FuncArg] Stmt SrcSpan
--Block    [Stmt] SrcSpan
compileStmt (Block stmts _) = do
  stmts' <- mapM compileStmt stmts
  return $ IR.Block stmts'
--Var      [(Identifer,Maybe Expr)] SrcSpan
--Exec     Expr SrcSpan
--Nop      SrcSpan

compileExpr :: Expr -> Compile IR.Node
--Bin      String Expr Expr SrcSpan
compileExpr (Bin op e1 e2 _) = do
  e1' <- compileExpr e1
  e2' <- compileExpr e2
  return $ IR.Bin e1' (T.pack op) e2'
--PreUni   String Expr SrcSpan
compileExpr (PreUni op e _) = do
  e' <- compileExpr e
  return $ IR.PreUni (T.pack op) e'
--PostUni  Expr String SrcSpan
compileExpr (PostUni e op _) = do
  e' <- compileExpr e
  return $ IR.PostUni e' (T.pack op)
--Tri      Expr Expr Expr SrcSpan
--Cast     Identifer Expr SrcSpan
--Int      Integer SrcSpan
--Real     Double SrcSpan
--Str      Text SrcSpan
--Ident    Identifer SrcSpan
--Array    [Expr] SrcSpan
--Dict     [(Expr, Expr)] SrcSpan
--AnonFunc [FuncArg] Stmt SrcSpan
--Index    Expr Expr SrcSpan
--Call     Expr [ApplyArg] SrcSpan
--Dot      Expr Identifer SrcSpan
--Null     SrcSpan
--WithThis SrcSpan

compileApplyArg :: ApplyArg -> IR.Node
compileApplyArg = undefined
--ApplyLeft
--ApplyArray Expr
--ApplyExpr Expr
--ApplyVoid

compileFuncArg :: FuncArg -> IR.Node
compileFuncArg = undefined
--data FuncArg =
--FuncLeft
--FuncArray Identifer
--FuncArg Identifer (Maybe Expr)
