{-# LANGUAGE LambdaCase #-}
module AssertionRemoval where

import Z3.Monad (AST, Z3, (+?))
import qualified Z3.Monad as Z3

import Control.Monad (foldM, (=<<), (<=<), (>>=))

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.List (find)
import Data.Maybe

import Syntax

type Vars = Map Ident AST

intSz = 32


makeVar :: Type -> Ident -> Z3 AST
makeVar t n =
  case t of
    IntegerT -> Z3.mkFreshBvVar (show n) intSz
    BooleanT -> Z3.mkFreshBoolVar (show n)


makeArr :: Type -> Ident -> Z3 AST
makeArr t n =
  case t of
    IntegerT -> do
      intSort <- Z3.mkBvSort intSz
      arrSort <- Z3.mkArraySort intSort intSort
      Z3.mkFreshConst (show n) arrSort
    BooleanT -> do
      boolSort <- Z3.mkBoolSort
      arrSort  <- Z3.mkArraySort boolSort boolSort
      Z3.mkFreshConst (show n) arrSort


setArray :: Vars -> [Expr] -> Int -> AST -> Z3 AST
setArray scope vals idx arr =
  case vals of
    [] -> return arr
    (hd:tl) -> do
      idx' <- Z3.mkFreshBvVar "arrIdx" intSz
      hd'  <- processExpr scope hd
      Z3.mkStore arr idx' hd' >>= (setArray scope tl (idx+1))


getVar :: Ident -> Vars -> AST
getVar n scope =
  case Map.lookup n scope of
    Just x -> x
    Nothing -> error $ "Variable " ++ show n ++ " could not be found!"


operate :: BinOp -> AST -> AST -> Z3 AST
operate opr e1 e2 =
  case opr of
    -- arithmetic operators
    Plus -> Z3.mkBvadd e1 e2
    Sub  -> Z3.mkBvsub e1 e2
    Xor  -> Z3.mkBvxor e1 e2
    Mul  -> Z3.mkBvmul e1 e2
    Div  -> Z3.mkBvudiv e1 e2 -- unsigned as integers in Japa are unsigned
    Modulo -> Z3.mkBvurem e1 e2 -- unsigned rem, as all integers are positive
    BAnd   -> Z3.mkBvand e1 e2
    BOr    -> Z3.mkBvor e1 e2
    -- boolean operators
    And    -> Z3.mkAnd [e1, e2]
    Or     -> Z3.mkOr [e1, e2]
    Great  -> Z3.mkBvugt e1 e2
    GreatEq-> Z3.mkBvuge e1 e2
    Less   -> Z3.mkBvult e1 e2
    LessEq -> Z3.mkBvule e1 e2
    NotEq  -> Z3.mkNot =<< Z3.mkEq e1 e2
    Eq     -> Z3.mkEq e1 e2


processExpr :: Vars -> Expr -> Z3 AST
processExpr scope e =
  case e of
    ConstE v ->
      case v of
        IntegerV i -> Z3.mkBvNum intSz i
        BooleanV b -> Z3.mkBool b
    VarE var ->
      case var of
        LVar n ->
          return $ getVar n scope
        _ -> error "VarE for array not implemented yet"
    Arith op e1 e2 -> do
      e1' <- processExpr scope e1
      e2' <- processExpr scope e2
      operate op e1' e2'
    Not e -> do
      e' <- processExpr scope e
      Z3.mkNot e'
    _ -> error $ "Optimization expression: Expr not implemented yet " ++ show e


createITE :: AST -> Vars -> Vars -> Vars -> Z3 Vars
createITE cond orig scope1 scope2 = foldM f orig $ Map.keys orig
  where f scope var = do
          let var1  = getVar var scope1
          let var2  = getVar var scope2
          newVar    <- makeVar (IntegerT) var -- TODO: again a hack with the type
          ite       <- Z3.mkIte cond var1 var2
          Z3.assert =<< Z3.mkEq newVar ite
          return $ Map.insert var newVar scope


validate :: Z3 Bool
validate = do
  val <- Z3.solverCheck
  case val of
    Z3.Sat -> return False
    Z3.Unsat -> return True


-- Processes stmt in given ast and scope, and returns (updated scope, optimized stmt)
-- Try optimize when:
--    * dealloc is met
--    * ite is met
--    * for loop is met
processStatement :: [ProcDecl] -> Vars -> Stmt -> Z3 (Vars, Stmt)
processStatement ast scope stmt =
  case stmt of
    Global (Var t name val _) _ -> do
      newVar <- makeVar (fromJust t) name
      newVal <- processExpr scope (fromJust val)
      Z3.assert =<< Z3.mkEq newVar newVal
      return (Map.insert name newVar scope, stmt)

    Global (Arr t name sz vals _) _ -> do
      newVar  <- makeArr (fromJust t) name
      newVar' <- setArray scope (fromJust vals) 0 newVar
      return (Map.insert name newVar' scope, stmt)

    Local (Var t name val _) _ -> do
      newVar <- makeVar (fromJust t) name
      newVal <- processExpr scope (fromJust val)
      Z3.assert =<< Z3.mkEq newVar newVal
      return (Map.insert name newVar scope, stmt)

    DLocal (Var t name val _) _ -> do
      val     <- processExpr scope (fromJust val)
      let var = getVar name scope
      eq <- Z3.mkEq var val
      Z3.push
      Z3.assert =<< Z3.mkNot eq
      validated <- validate
      if (validated) then Z3.pop 1 >> return (scope, Skip)
      else                Z3.pop 1 >> Z3.assert eq >> return (scope, stmt)

    Mod (Moderator (Var _ name _ _) op val) _ _ -> do
      -- Moderation can only happen for integer types. This should be checked by a type checker
      let oldVar  = getVar name scope
      newVar      <- makeVar IntegerT name
      incrVal     <- processExpr scope val
      newVal      <-
        case op of
          PlusEq -> Z3.mkBvadd oldVar incrVal
          SubEq  -> Z3.mkBvsub oldVar incrVal
          XorEq  -> Z3.mkBvxor oldVar incrVal
      Z3.assert =<< Z3.mkEq newVar newVal
      return (Map.insert name newVar scope, stmt)

    Mod (Moderator (Arr _ name sz vals _) op val) idx _ -> do
      let arr = getVar name scope
      incrVal <- processExpr scope val
      idx'    <- processExpr scope (head (fromJust idx)) -- TODO: Only works for 1D array
      oldVal  <- Z3.mkSelect arr idx'
      newVal  <-
        case op of
          PlusEq -> Z3.mkBvadd oldVal incrVal
          SubEq  -> Z3.mkBvsub oldVal incrVal
          XorEq  -> Z3.mkBvxor oldVal incrVal
      Z3.mkStore arr idx' newVal
      return (scope, stmt)

    Switch (Var t1 n1 _ _) (Var t2 n2 _ _) _ -> do
      newVar1 <- makeVar (fromJust t1) n1 -- TODO: This requires a type check phase to annotate with types
      newVar2 <- makeVar (fromJust t2) n2
      let n1' =  getVar n1 scope
      let n2' =  getVar n2 scope
      Z3.assert =<< Z3.mkEq newVar1 n2'
      Z3.assert =<< Z3.mkEq newVar2 n1'
      return (Map.insert n1 newVar1 $ Map.insert n2 newVar2 scope, stmt)

    Ite ifcond body1 ficond body2 pos -> do
      {-
        1. validate if body
        2. validate fi cond
        3. validate else body
        4. make ITE continuation
      -}
      -- if part
      ifcond' <- processExpr scope ifcond
      Z3.push
      Z3.assert ifcond'
      body1'  <- processStatements ast scope body1
      ficond' <- processExpr (fst body1') ficond

      -- fi part
      Z3.assert =<< Z3.mkNot ficond'
      fiValidated <- validate
      Z3.pop 1

      -- else part
      Z3.push
      Z3.assert =<< Z3.mkNot ifcond'
      body2' <- processStatements ast scope body2
      Z3.pop 1

      -- Continuation
      ite <- createITE ifcond' scope (fst body1') (fst body2')
      return (ite,
        Ite ifcond (snd body1') (if fiValidated then SkipE else ficond) (snd body2') pos)

    Assert e _ -> do -- TODO: make this work like discussed
      cond <- processExpr scope e

      Z3.push
      Z3.assert =<< Z3.mkNot cond
      validated <- validate
      Z3.pop 1

      Z3.assert cond >> return (scope, stmt)

    Call name args pos -> error "Optimization statement: Call not implemented yet"
    Uncall name args pos -> error "Optimization statement: Uncall not implemented yet"
    For1 var body mod cond pos -> error "Optimization statement: For1 not implemented yet"
    For2 var mod body cond pos -> error "Optimization statement: For2 not implemented yet"

    Skip -> return (scope, Skip)
    _ -> error $ "Optimization statement: Stmt not implemented yet - " ++ show stmt


processStatements :: [ProcDecl] -> Vars -> [Stmt] -> Z3 (Vars, [Stmt])
processStatements ast scope body = foldM (f ast) (scope, []) body
  where
    f :: [ProcDecl] -> (Vars, [Stmt]) -> Stmt -> Z3 (Vars, [Stmt])
    f ast (scope, acc) stmt = do
      res <- processStatement ast scope stmt
      return (fst res, acc ++ [snd res])


processArgs :: [FArg] -> Z3 Vars
processArgs args = foldM f Map.empty args
  where f vars arg =
          case arg of
            VarFA t n _ -> do
              newVar <- makeVar t n
              return $ Map.insert n newVar vars
            ArrFA t n _ _ -> do
              newArr <- makeArr t n
              return $ Map.insert n newArr vars


processProcedure :: [ProcDecl] -> Vars  -> ProcDecl -> Z3 ProcDecl
processProcedure ast scope (ProcDecl name args body pos) = do
  initialScope <- processArgs args
  body' <- processStatements ast initialScope body
  return $ ProcDecl name args (snd body') pos


-- TODO: Should start with main procedure, and then every global should be added to the start scope
--       given to every procedure.
processProgram :: Program -> Z3 Program
processProgram (Program decls) = do
  decls' <- mapM (processProcedure (tail decls) Map.empty) decls
  return $ Program decls'

-- show <$> evalZ3 (liftM snd (processStatements [] Map.empty (parseStatements "local int a = 2\na += 1\ndealloc int a = 3")))
