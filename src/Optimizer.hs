{-# LANGUAGE LambdaCase #-}
module Optimizer where

import Z3.Monad (AST, Z3, (+?))
import qualified Z3.Monad as Z3

import Control.Monad (foldM, (=<<), (<=<))

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Syntax

type Z3Var = AST
type Vars = Map Ident Z3Var

intSz = 32

makeVar :: Type -> Ident -> Z3 AST
makeVar t n =
  case t of
    IntegerT _ -> Z3.mkFreshBvVar (show n) intSz
    BooleanT _ -> Z3.mkFreshBoolVar (show n)

makeArr :: Type -> Ident -> Z3 AST
makeArr t n =
  case t of
    IntegerT _ -> do
      intSort <- Z3.mkBvSort intSz
      arrSort <- Z3.mkArraySort intSort intSort
      Z3.mkFreshConst (show n) arrSort
    BooleanT _ -> do
      boolSort <- Z3.mkBoolSort
      arrSort  <- Z3.mkArraySort boolSort boolSort
      Z3.mkFreshConst (show n) arrSort

getVar :: Ident -> Vars -> Z3Var
getVar n scope =
  case Map.lookup n scope of
    Just x -> x
    Nothing -> error $ "Variable " ++ show n ++ " could not be found!" ++ (concat $ map show $ Map.keys scope)

processArgs :: [FArg] -> Z3 Vars
processArgs args = foldM f Map.empty args
  where f vars arg =
          case arg of
            VarF t n _ -> do
              newVar <- makeVar t n
              return $ Map.insert n newVar vars
            Arr t n _ -> do
              newArr <- makeArr t n
              return $ Map.insert n newArr vars

findMark :: [Stmt] -> Bool
findMark [] = False
findMark (hd:tl) =
  case hd of
    Mark _ -> True
    _      -> findMark tl


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
    Great  -> Z3.mkGt e1 e2
    GreatEq-> Z3.mkGe e1 e2
    Less   -> Z3.mkLt e1 e2
    LessEq -> Z3.mkLe e1 e2
    NotEq  -> Z3.mkNot =<< Z3.mkEq e1 e2
    Eq     -> Z3.mkEq e1 e2

processExpr :: Vars -> Expr -> Z3 AST
processExpr scope e =
  case e of
    ConstE v _ ->
      case v of
        IntegerV i _ -> Z3.mkBvNum intSz i
        BooleanV b _ -> Z3.mkBool b
    VarE var _ ->
      case var of
        Var n _ ->
          return $ getVar n scope
        _ -> error "VarE for array not implemented yet"
    Arith e1 op e2 _ -> do
      e1' <- processExpr scope e1
      e2' <- processExpr scope e2
      operate op e1' e2'
    _ -> error "Expr not implemented yet"

processLastStmt :: Vars -> Stmt -> Z3 Vars
processLastStmt scope stmt =
  case stmt of
    DLocal t n e _ -> let var = getVar n scope in do
      newVal <- processExpr scope e
      eq <- Z3.mkEq var newVal
      Z3.assert =<< Z3.mkNot eq
      return scope
    Ite ifcond body ficond body2 _ -> do
      case findMark body of
        True -> do
          -- final stmt in body
          Z3.assert =<< processExpr scope ifcond
          processBody (return scope) body
        False ->
          case findMark body2 of
            True -> do
              -- final stmt in body2
              ifcond' <- processExpr scope ifcond
              Z3.assert =<< Z3.mkNot ifcond'
              processBody (return scope) body2
            False -> do
              -- final stmt is this if!
              Z3.assert =<< processExpr scope ifcond
              processBody (return scope) body
              Z3.assert =<< Z3.mkNot =<< processExpr scope ficond
              return scope
    _ -> error "Stmt not implemented for last stmt yet"

processBody :: Z3 Vars -> [Stmt] -> Z3 Vars
processBody scope body =
  case body of
    (hd:tl) -> processBody (f scope hd) tl
    []      -> scope
 where f scope stmt =
         case stmt of
           Local t n e _ -> do
             newVar <- makeVar t n
             scope' <- scope
             newVal <- processExpr scope' e
             Z3.assert =<< Z3.mkEq newVar newVal
             return $ Map.insert n newVar scope'
           DLocal t n e _ -> do
             scope' <- scope
             val    <- processExpr scope' e
             let var= getVar n scope'
             Z3.assert =<< Z3.mkEq var val
             return scope'
           Mod (Moderator n opr e p) _ -> do
             scope' <- scope
             newVar <- makeVar (IntegerT p) n
             newVal <- processExpr scope' e
             Z3.assert =<< Z3.mkEq newVar newVal
             return $ Map.insert n newVar scope'
           Switch n1 n2 p -> do
             -- This is a hack, as I don't have any type checking in the compiler
             -- alternatively make the type explicitly written
             scope'  <- scope
             newVar1 <- makeVar (IntegerT p) n1
             newVar2 <- makeVar (IntegerT p) n2
             let n1' = getVar n1 scope'
             let n2' = getVar n2 scope'
             Z3.assert =<< Z3.mkEq newVar1 n1'
             Z3.assert =<< Z3.mkEq newVar2 n2'
             return $ Map.insert n1 newVar1 $ Map.insert n2 newVar2 scope'
           Ite ifcond body1 ficond body2 _ -> do
             -- as the Ite is not marked, the marked statement comes in the tail of the list
             -- idea: forall Mod of v in body1 create ite(ifcond v' v ve) where ve is the
             --       modification from body2 or from before this Ite.
             scope' <- scope
             scope1 <- processBody scope body1
             scope2 <- processBody scope body2
             cond'  <- processExpr scope' ifcond
             createITE cond' scope' scope1 scope2
           Skip -> scope
           Mark stmt     -> do
             scope' <- scope
             processLastStmt scope' stmt
           _ -> error "Stmt not implementet yet"

createITE :: AST -> Vars -> Vars -> Vars -> Z3 Vars
createITE cond orig scope1 scope2 = foldM f orig $ Map.keys orig
  where f scope var = do
          let var1  = getVar var scope1
          let var2  = getVar var scope2
          newVar    <- makeVar (IntegerT (Pos 1 1)) var -- again a hack with the type
          ite       <- Z3.mkIte cond var1 var2
          Z3.assert =<< Z3.mkEq newVar ite
          return $ Map.insert var newVar scope


-- validateProcedure :: [ProcDecl] -> Z3 Vars
validateProcedure procs =
  case procs of
    ((ProcDecl n args body):tl) -> putStrLn <=< runZ3 $ do
      processBody (processArgs args) body
      Z3.solverCheckAndGetModel >>= \case
        (Z3.Sat, Just model) -> Z3.modelToString model
        (Z3.Unsat, _) -> return "Optimize"
    _ -> error "Yeeeeah, this is not the way to use this..."
 where runZ3 = Z3.evalZ3

