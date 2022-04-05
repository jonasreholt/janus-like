{-# LANGUAGE LambdaCase #-}
module Optimizer where

import Z3.Monad (AST, Z3, (+?))
import qualified Z3.Monad as Z3

import Control.Monad

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.List (find)

import Syntax2

type Z3Var = AST
type Vars = Map Ident Z3Var

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

getVar :: Ident -> Vars -> Z3Var
getVar n scope =
  case Map.lookup n scope of
    Just x -> x
    Nothing -> error $ "Variable " ++ show n ++ " could not be found!" ++ (concat $ map show $ Map.keys scope)

processArgs :: [FArg] -> Z3 Vars
processArgs args = foldM f Map.empty args
  where f vars arg =
          case arg of
            VarFA t n _ -> do
              newVar <- makeVar t n
              return $ Map.insert n newVar vars
            ArrFA t n _ -> do
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
        IntegerV i -> Z3.mkBvNum intSz i
        BooleanV b -> Z3.mkBool b
    VarE var _ ->
      case var of
        LVar n ->
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
    DLocal (Var t n _) e _ -> let var = getVar n scope in do
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
           Local (Var t n _) e _ -> do
             newVar <- makeVar t n
             scope' <- scope
             newVal <- processExpr scope' e
             Z3.assert =<< Z3.mkEq newVar newVal
             return $ Map.insert n newVar scope'
           DLocal (Var t n _) e _ -> do
             scope' <- scope
             val    <- processExpr scope' e
             let var= getVar n scope'
             Z3.assert =<< Z3.mkEq var val
             return scope'
           Mod (Moderator x opr e1) _ _ ->
             case x of
               Var t n _ -> do
                 scope' <- scope
                 newVar <- makeVar t n
                 newVal <- processExpr scope' e1
                 Z3.assert =<< Z3.mkEq newVar newVal
                 return $ Map.insert n newVar scope'
               Arr t n s _ -> error "Mod not implemented for arrays yet"
           Switch (Var t1 n1 _) (Var t2 n2 _) p -> do
             scope'  <- scope
             newVar1 <- makeVar t1 n1
             newVar2 <- makeVar t2 n2
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
          newVar    <- makeVar (IntegerT) var -- again a hack with the type
          ite       <- Z3.mkIte cond var1 var2
          Z3.assert =<< Z3.mkEq newVar ite
          return $ Map.insert var newVar scope

unroll :: [Stmt] -> [Stmt]
unroll = \case
  [] -> []
  (For1 var e1 body mod e2 _):tl -> error "For1 not implemented yet"
  (For2 var e1 mod body e2 _):tl -> error "For2 not implemented yet"
  hd:tl -> hd : unroll tl


inline :: [ProcDecl] -> [Stmt] -> [Stmt]
inline procs = \case
    [] -> []
    ((Call name args _):tl) ->
      case findProc name procs of
        ProcDecl n fargs b _ ->
          (inlineArgs args fargs) ++ inline procs b ++ inline procs tl
    ((Uncall name args _):tl) ->
      case findProc name procs of
        ProcDecl n fargs b _ ->
          (inlineArgs args fargs) ++ b ++ inline procs tl
    (hd:tl) ->
      hd : (inline procs tl)
  where findProc :: Ident -> [ProcDecl] -> ProcDecl
        findProc n procs =
          case find (\(ProcDecl n' _ _ _) -> n' == n) procs of
            Just p -> p
            Nothing -> error "Could not find function to inline"
        inlineArgs :: [AArg] -> [FArg] -> [Stmt]
        inlineArgs aargs fargs =
          case aargs of
            [] -> []
            ((ConstAA v p):tl) ->
              case head fargs of
                ConstFA t n _ ->
                  (Local (Var t n p) (ConstE v p) p) : inlineArgs tl (tail fargs)
            ((VarAA v@(Var _ n1 p1) p2):tl) ->
              case head fargs of
                (VarFA t n2 _) ->
                  (Local (Var t n2 p2) (VarE (LVar n1) p1) p2) : inlineArgs tl (tail fargs)
            (_:tl) -> inlineArgs tl $ tail fargs

inlineUnroll :: [ProcDecl] -> [Stmt] -> [Stmt]
inlineUnroll procs body = foldr (f procs) [] (reverse body)
  where f :: [ProcDecl] -> Stmt -> [Stmt] -> [Stmt]
        f procs stmt accBody =
          case stmt of
            For1 var e1 body mod e2 _ -> error "For1 not implemented yet"
            For2 var e1 mod body e2 _ -> error "For2 not implemented yet"
            Call name args _ ->
              case findProc name procs of
                ProcDecl name' fargs body' _ ->
                  let inlinedArgs = g args fargs in
                    let endArgsInline = h args fargs in
                      accBody ++ inlinedArgs ++ body' ++ endArgsInline
--                    accBody ++ inlinedArgs ++ h vars body' -- THIS MATCHES THE COMMENTED CODE
            Uncall name args _ ->
              case findProc name procs of
                ProcDecl name' fargs body' _ ->
                  let inlinedArgs = g args fargs in
                    let endArgsInline = h args fargs in
                      accBody ++ inlinedArgs ++ body' ++ endArgsInline
            stmt -> accBody ++ [stmt]
           where findProc :: Ident -> [ProcDecl] -> ProcDecl
                 findProc n procs =
                   case find (\(ProcDecl n' _ _ _) -> n' == n) procs of
                     Just p -> p
                     Nothing -> error "Could not find function to inline"

                 g :: [AArg] -> [FArg] -> [Stmt]
                 g args fargs =
                   case args of
                     [] -> []
                     (ConstAA val p):tl ->
                       case fargs of
                         (ConstFA t n _):ftl ->
                           (Local (Var t n p) (ConstE val p) p) : g tl ftl
                         _ -> error "Actual parameter does not match formal"
                     (VarAA (Var _ n1 p1) p2):tl ->
                       case fargs of
                         (VarFA t n2 _):ftl ->
                           (Local (Var t n2 p2) (VarE (LVar n1) p1) p2) : g tl ftl
                         _ -> error "Actual parameter does not match formal"
                 h :: [AArg] -> [FArg] -> [Stmt]
                 h args fargs =
                   case args of
                     [] -> []
                     (VarAA (Var _ n1 p1) p2):tl ->
                       case fargs of
                         (VarFA t n2 _):ftl ->
                           (Local (Var t n1 p1) (VarE (LVar n2) p1) p2) : h tl ftl
                         _ -> error "Actual parameter does not match formal"
                     _ -> h (tail args) (tail fargs)
               -- BELOW IS CREATING NEW LOCAL FOR EACH CONSTANT,
               -- AND THEN DOING SUBSTITUTION THROUGHOUT FUNCTION BODY
        -- where g :: [AArg] -> [FArg] -> [Stmt]
        --       g args fargs =
        --         case args of
        --           [] -> []
        --           (ConstAA val p):tl ->
        --             case fargs of
        --               (ConstFA t n _):ftl ->
        --                 (Local (Var t n p) (ConstE val p) p) : g tl ftl
        --           _ -> g (tail args) (tail fargs)
        --       h ::  Map Var Var -> [Stmt] -> [Stmt]
              -- h scope body =
              --   case body of
              --     [] -> []
              --     head:tl ->
              --       case head of
              --         Local var e p -> do
              --           e' <- replaceE scope e
              --           scope' <- Map.remove var scope
              --           (Local var e' p) : h scope' tl
              --         _ ->
              --           replaceS scope head : h scope tl
              -- where replaceS :: Map Var Var -> Stmt -> Stmt
              --       replaceS scope = \case
              --         DLocal v e p -> DLocal v (replaceE scope e) p
              --         Mod (Moderator var opr e) e2 p ->
              --           Mod (Moderator (replaceV scope var) opr (replaceE scope e)) (replaceE e2) p
              --         Switch var1 var2 p ->
              --           let lam = replaceV scope in
              --             Switch (lam var1) (lam var2) p
              --         Ite e1 body1 e2 body2 p ->
              --           Ite (replaceE scope e1) (h scope body1) (replaceE scope e2) (h scope body2) p
              --         For1 var e1 body (Moderator var opr e3) e2 p -> do
              --           e1' <- replaceE scope e1
              --           scope' <- Map.remove var scope
              --           body' <- h scope' body
              --           e2' <- replaceE scope' e2
              --           e3' <- replaceE scope' e2
              --           For1 var e1' body' (Moderator var opr e3') e2' p
              --         For2 var e1 (Moderator var opr e3) body e2 p -> do
              --           e1' <- replaceE scope e1
              --           scope' <- Map.remove var scope
              --           body' <- h scope' body
              --           e2' <- replaceE scope' e2
              --           e3' <- replaceE scope' e3
              --           For2 var e1' (Moderator var opr e3') body' e2' p
              --         Call n args p ->
              --           Call n (replaceA scope args) p
              --         Uncall n args p ->
              --           Uncall n (replaceA scope args) p
              --         stmt -> stmt

              --       replaceE :: Map Var Var -> Expr -> Expr
              --       replaceE scope = \case
              --         VarE lvar p ->

              --       replaceV :: Map Var Var -> Var -> Var
              --       replaceA :: Map Var Var -> [AArg] -> [AArg]


-- validateProcedure :: [ProcDecl] -> Z3 Vars
validateProcedure procs =
  case procs of
    ((ProcDecl n args body _):tl) -> putStrLn <=< runZ3 $ do
      let body' = inlineUnroll tl body
      processBody (processArgs args) body'
      Z3.solverCheckAndGetModel >>= \case
        (Z3.Sat, Just model) -> Z3.modelToString model
        (Z3.Unsat, _) -> return "Optimize"
    _ -> error "Yeeeeah, this is not the way to use this..."
 where runZ3 = Z3.evalZ3

