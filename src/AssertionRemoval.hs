{-# LANGUAGE LambdaCase #-}
module AssertionRemoval where

import Prelude hiding (mod)

import Z3.Monad (AST, Z3, (+?))
import qualified Z3.Monad as Z3

import Control.Monad (foldM, (=<<), (<=<), (>>=))

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.List (find, nub, (\\))
import Data.Maybe
import Data.Either

import Debug.Trace

import Syntax
import AstReversing
import EvalExpr
import RenameProcedure (renameProc)

type Vars = Map Ident (AST, Var)
type Bound = Int

intSz = 32
unrollBound = 100


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
      idxSort  <- Z3.mkBvSort intSz
      arrSort  <- Z3.mkArraySort idxSort boolSort
      Z3.mkFreshConst (show n) arrSort


-- If Nothing is returned the array being set should be invalidated
setArray :: Vars -> [Expr] -> Int -> AST -> Z3 (Either Ident AST)
setArray scope vals idx arr =
  case vals of
    [] -> do
      return $ Right arr
    (hd:tl) -> do
      idx' <- Z3.mkInt idx =<< Z3.mkBvSort intSz
      hd'  <- processExpr scope hd
      case hd' of
        Right x ->
          Z3.mkStore arr idx' x >>= (setArray scope tl (idx+1))
        otherwise -> return otherwise


invalidateVar :: Vars -> Ident -> Z3 Vars
invalidateVar scope idnt = case tryGetVar idnt scope of
  Just (_, var) -> do
    generalizedVar <-
      (case var of
         Var (Just t) _ _ _ -> makeVar t idnt
         Arr (Just t) _ _ _ _ -> makeArr t idnt)
    return $ Map.insert idnt (generalizedVar, var) scope
  Nothing ->
    -- Happens when invalidating modified vars if the var is declared within the [stmt]
    -- invalidating
    return scope

invalidateVars :: Vars -> [Ident] -> Z3 Vars
invalidateVars scope vars =
  foldM invalidateVar scope vars

modifiedVars :: [ProcDecl] -> [Stmt] -> [Ident]
modifiedVars ast = \case
  [] -> []

  (Mod (Moderator (Var _ name _ _) _ _) _ _:tl)   -> name : modifiedVars ast tl
  (Mod (Moderator (Arr _ name _ _ _) _ _) _ _:tl) -> name : modifiedVars ast tl

  (Switch (Var _ name1 _ _) (Var _ name2 _ _) _:tl)     -> name1 : name2 : modifiedVars ast tl
  (Switch (Arr _ name1 _ _ _) (Arr _ name2 _ _ _) _:tl) -> name1 : name2 : modifiedVars ast tl

  (Ite _ bodyif _ bodyelse _:tl) -> modifiedVars ast bodyif ++ modifiedVars ast bodyelse ++ modifiedVars ast tl

  (For1 _ _ body (Moderator var _ _) _ _ _:tl) -> getVarName var : modifiedVars ast body ++ modifiedVars ast tl
  (For2 _ _ (Moderator var _ _) body _ _ _:tl) -> getVarName var : modifiedVars ast body ++ modifiedVars ast tl

  (Call n _ _:tl)   -> modifiedVars ast (recurseIntoProcBody n) ++ modifiedVars ast tl
  (Uncall n _ _:tl) -> modifiedVars ast (recurseIntoProcBody n) ++ modifiedVars ast tl

  (_:tl) -> modifiedVars ast tl
  where
    recurseIntoProcBody :: Ident -> [Stmt]
    recurseIntoProcBody n =
      let p = fromJust $ find (\(ProcDecl name _ _ _) -> name == n) ast in
        let (_, p', _) = renameProc Map.empty p 0 in
          getProcDeclBody p'

--Z3 (Map Ident Ident, ProcDecl, Int)


exprVars :: Expr -> [Ident]
exprVars = f []
  where
    f :: [Ident] -> Expr -> [Ident]
    f acc = \case
      Arith _ e1 e2 -> let acc' = f acc e1 in f acc' e2
      Not e         -> f acc e
      VarE (LVar name)     -> name:acc
      VarE (Lookup name _) -> name:acc
      _ -> acc


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


-- | Takes indexes and size of array and produces index into flattened array
calculateIndex :: Vars -> [Expr] -> [Integer] -> Z3 AST
calculateIndex scope idxs sz = case idxs of
  [last] ->
    processExpr scope last >>= \case
      Right last' -> return last'
      Left n -> error $ "calculating index failed " ++ show last ++ " in \n" ++ show scope
  (hd:tl) -> do
    idx <- Z3.mkBvNum intSz $ arrSize $ tail sz
    processExpr scope hd >>= \case
      Right hd' -> do
        idx' <- Z3.mkBvmul idx hd'
        rest <- calculateIndex scope tl (tail sz)
        Z3.mkBvadd idx' rest
      Left n -> error ""


-- | Given scope and an Expr it returns either culprit Ident of why it could
--   not be calculated, or it returns the z3 AST representing the given Expr
processExpr :: Vars -> Expr -> Z3 (Either Ident AST)
processExpr scope e =
  case e of
    ConstE v ->
      case v of
        IntegerV i -> Z3.mkBvNum intSz i >>= (\val -> return $ Right val)
        BooleanV b -> Z3.mkBool b >>= (\val -> return $ Right val)

    VarE (LVar n) -> case tryGetVar n scope of
      Just (n', _) -> return $ Right n'
      Nothing -> return $ Left n

    VarE (Lookup n es) -> case tryGetVar n scope of
      Just (n', Arr t _ (Just sz) _ _) -> do
        idx <- calculateIndex scope es sz
        val <- Z3.mkSelect n' idx
        return $ Right val
      Nothing -> return $ Left n

    Arith op e1 e2 -> do
      e1' <- processExpr scope e1
      e2' <- processExpr scope e2
      case (e1', e2') of
        (Right x, Right y) ->
          operate op x y >>= (\val -> return $ Right val)
        (Left n, _) ->
          return (Left n)
        (_, Left n) ->
          return (Left n)

    Not e -> do
      e' <- processExpr scope e
      case e' of
        Right x ->
          Z3.mkNot x >>= (\val -> return $ Right val)

        Left n -> return (Left n)

    Size (Lookup n idx) _ -> case tryGetVar n scope of
      Just (varz3, Arr t n s e _) ->
        let size = arrSize $ take (length idx) (fromJust s) in
          Z3.mkBvNum intSz size >>= (\val -> return $ Right val)

      Nothing -> return (Left n)

    Size (LVar n) _ -> case tryGetVar n scope of
      Just (varz3, Arr t n s e _) ->
        let size = arrSize $ fromJust s in
          Z3.mkBvNum intSz size >>= (\val -> return $ Right val)
      Nothing ->
        return $ Left n

    _ -> error $ "Optimization expression: Expr not implemented yet " ++ show e


createITE :: AST -> Vars -> Vars -> Vars -> Z3 Vars
createITE cond orig scope1 scope2 = foldM f orig $ Map.keys orig
  where f :: Vars -> Ident -> Z3 Vars
        f scope name = do
          let var1 = tryGetVar name scope1
          let var2 = tryGetVar name scope2

          case (var1, var2) of
            (Just (var1z3, var), Just (var2z3, _)) -> do
              newVar <-
                (case var of
                   Var (Just t) _ _ _ ->
                     makeVar t name
                   Arr (Just t) _ _ _ _ ->
                     makeArr t name)
              ite    <- Z3.mkIte cond var1z3 var2z3
              Z3.assert =<< Z3.mkEq newVar ite
              return $ Map.insert name (newVar, var) scope

            -- else variable invalidated in some scope
            _ -> invalidateVar scope name


-- | Take Expr and renew variables used in scope
--   Handles it almost like declaring new variables
prepareAssert :: Vars -> Expr -> Z3 (Either Ident Vars)
prepareAssert scope expr = do
  processExpr scope expr >>= \case
    Right expr' -> do
      satisfied <- satisfiable expr'
      if satisfied
      then do Z3.assert expr'
              return $ Right scope
      else scopeExpr scope expr >>= \case
        Right scope' -> processExpr scope' expr >>= \case
          Right var -> do
            Z3.assert var
            return $ Right scope'
          Left errName -> return $ Left errName
        Left errName -> return $ Left errName
    Left errName -> return $ Left errName
  where
    scopeExpr :: Vars -> Expr -> Z3 (Either Ident Vars)
    scopeExpr scope = \case
      VarE (LVar n) -> f scope n
      VarE (Lookup n idxs) -> f scope n
      Arith _ e1 e2 ->
        scopeExpr scope e1 >>= \case
          Right scope' ->
            scopeExpr scope' e2
          otherwise -> return otherwise
      Not e -> scopeExpr scope e
      Size (LVar n) _ -> f scope n
      _ -> return $ Right scope

    f :: Vars -> Ident -> Z3 (Either Ident Vars)
    f scope name = case tryGetVar name scope of
      Just (_, var) -> do
        var' <-
          (case var of
            Var (Just t) _ _ _ -> makeVar t name
            Arr (Just t) _ _ _ _ -> makeArr t name)
        return $ Right $ Map.insert name (var', var) scope
      Nothing -> return $ Left name


mod :: Stmt -> Vars -> [String] -> Z3 (Vars, Stmt, [String])
mod stmt@(Mod (Moderator var@(Var t name _ _) op val) _ _) scope warnings = do
  case tryGetVar name scope of
    Just (oldVar, _) -> do
      newVar      <- makeVar (fromJust t) name
      processExpr scope val >>= \case
        Right incrVal -> do
          newVal <-
            case op of
              PlusEq -> Z3.mkBvadd oldVar incrVal
              SubEq  -> Z3.mkBvsub oldVar incrVal
              XorEq  -> Z3.mkBvxor oldVar incrVal
          Z3.assert =<< Z3.mkEq newVar newVal
          return (Map.insert name (newVar, var) scope, stmt, warnings)

        Left errvar -> do
          let errmsg = show name ++ " invalidated because of " ++ show errvar
          scope' <- invalidateVar scope name
          return (scope', stmt, errmsg:warnings)

    _ ->
      return (scope, stmt, (show name ++ " used invalidated var") : warnings)

mod _ _ _ = error "Please only use this function for processing modification!"


data Direction = Ascending | Descending | Unknown
  deriving (Show)



satisfiable :: AST -> Z3 Bool
satisfiable expr = do
  Z3.push
  Z3.assert expr
  validated <- Z3.solverCheck
  ------------------------
  -- odin <- Z3.solverToString
  -- trace (show odin) $return()
  ------------------------
  Z3.pop 1
  case validated of
    Z3.Sat   -> return True
    Z3.Unsat -> return False


validateArray :: Stmt -> AST -> [Expr] -> Vars -> Z3 (Either String Bool)
validateArray stmt arr vals scope = do
  Z3.push
  -- Checking whether the constraint is satisfiable
  foldM (f arr scope) 0 vals
  val <- Z3.solverCheck
  Z3.pop 1
  case val of
    Z3.Sat -> do
      Z3.push
      foldM (g arr scope) 0 vals
      val' <- Z3.solverCheck
      Z3.pop 1
      case val' of
        Z3.Sat -> return $ Right False
        Z3.Unsat -> return $ Right True
    Z3.Unsat -> return (Left $ "Z3 expression is a fallacy: " ++ show stmt)
  where
    f :: AST -> Vars -> Int -> Expr -> Z3 Int
    f arr scope idx expr = do
      idx' <- Z3.mkInt idx =<< Z3.mkBvSort intSz
      processExpr scope expr >>= \case
        Right newVal -> do
          sel <- Z3.mkSelect arr idx'
          eq  <- Z3.mkEq newVal sel
          Z3.assert eq
          return $ idx + 1
        Left errvar -> error "ahh array val could not be processed during validation"

    g :: AST -> Vars -> Int -> Expr -> Z3 Int
    g arr scope idx expr = do
      idx' <- Z3.mkInt idx =<< Z3.mkBvSort intSz
      processExpr scope expr >>= \case
        Right newVal -> do
          sel <- Z3.mkSelect arr idx'
          eq  <- Z3.mkEq newVal sel
          Z3.assert =<< Z3.mkNot eq
          return $ idx + 1
        Left errvar -> error "ahh array val could not be processed during validation"

validate :: Show a => a -> AST -> Z3 (Either String Bool)
validate sinner expr = do
  Z3.push
  Z3.assert expr
  val <- Z3.solverCheck
  Z3.pop 1
  -----------------------
  -- (if show sinner == show "av"
  --   then do
  --     allah <- Z3.solverToString
  --     trace ("\nallah\n"++show sinner++"\n"++show allah) $return ()
  --   else return ())
  -----------------------
  case val of
    Z3.Sat -> do
      Z3.push
      Z3.assert =<< Z3.mkNot expr
      val' <- Z3.solverCheck
      Z3.pop 1
      case val' of
        Z3.Sat -> return $ Right False
        Z3.Unsat -> return $ Right True

    Z3.Unsat -> return (Left $ "Z3 expression is a fallacy: " ++ show sinner)

    Z3.Undef -> return (Left $ "Z3 expression is unknown: " ++ show sinner)


-- Processes stmt in given ast and scope, and returns (updated scope, optimized stmt)
-- Try optimize when:
--    * dealloc is met
--    * ite is met
--    * for loop is met
processStatement :: Bool -> [ProcDecl] -> Vars -> Stmt -> Int -> Bound -> [String] ->
  Z3 (Vars, Stmt, Bound, [String])
processStatement doOpt ast scope stmt state bound warnings =
  case stmt of
    Global var@(Var t name val _) _ -> do
      newVar <- makeVar (fromJust t) name

      processExpr scope (fromJust val) >>= \case
        Right newVal -> do
          Z3.assert =<< Z3.mkEq newVar newVal
          return (Map.insert name (newVar, var) scope, stmt, bound, warnings)

        Left errvar ->
          let errmsg = show name ++ " used with invalid variable " ++ show errvar in
            return (scope, stmt, bound, errmsg:warnings)

    Global var@(Arr t name (Just sz) vals _) _ -> do
      newVar  <- makeArr (fromJust t) name
      let vals' = (case vals of
                 Just v -> v
                 Nothing -> replicate (fromIntegral (arrSize sz)) (ConstE (IntegerV 0)))
      setArray scope vals' 0 newVar >>= \case
        Right newVar' ->
          return (Map.insert name (newVar', var) scope, stmt, bound, warnings)

        Left errvar ->
          let errmsg = show name ++ " used with invalid variable " ++ show errvar in
            return (scope, stmt, bound, errmsg:warnings)

    Local var@(Var t name val _) _ -> do
      newVar <- makeVar (fromJust t) name

      processExpr scope (fromJust val) >>= \case
        Right newVal -> do
          Z3.assert =<< Z3.mkEq newVar newVal
          return (Map.insert name (newVar, var) scope, stmt, bound, warnings)

        Left errvar ->
          let errmsg = show name ++ " used with invalid variable " ++ show errvar in
            return (scope, stmt, bound, errmsg:warnings)

    Local var@(Arr t name (Just sz) vals _) _ -> do
      newVar <- makeArr (fromJust t) name
      let vals' = (case vals of
                 Just v -> v
                 _ -> replicate (fromIntegral (arrSize sz)) (ConstE (IntegerV 0)))
      setArray scope vals' 0 newVar >>= \case
        Right newVar' ->
          return (Map.insert name (newVar', var) scope, stmt, bound, warnings)

        Left errvar ->
          let errmsg = show name ++ " used with invalid variable " ++ show errvar in
            return (scope, stmt, bound, errmsg:warnings)

    DLocal (Var t name val _) _ ->
      if (doOpt) then
        processExpr scope (fromJust val) >>= \case
          Right val ->
            case tryGetVar name scope of
              Just (varz3, var) -> do
                eq <- Z3.mkEq varz3 val
                validated <- validate stmt eq
                case validated of
                  Left err    ->
                    -- Continue without the assertion having any effect on the analysis
                    return (scope, stmt, bound, err:warnings)
                  Right True  -> return (scope, Skip, bound, warnings)
                  Right False -> Z3.assert eq >> return (scope, stmt, bound, warnings)

              Nothing ->
                -- The deallocated variable is invalidated so the information cannot
                -- be used further on
                return (scope, stmt, bound, (show name ++ " has been invalidated") : warnings)

          Left errvar ->
            let errmsg = show name ++ " used with invalid variable " ++ show errvar in
              return (scope, stmt, bound, errmsg:warnings)

      else
        return (scope, stmt, bound, warnings)

    DLocal (Arr t name sz vals _) _ ->
      if (doOpt) then
        case tryGetVar name scope of
          Just (varz3, var) -> do
            newVar <- makeArr (fromJust t) name
            setArray scope (fromJust vals) 0 newVar >>= \case
              Right newVar' -> do
                validated <- validateArray stmt varz3 (fromJust vals) scope
                case validated of
                  Left err ->
                    -- Continue without the assertion having any effect on analysis.
                    return (scope, stmt, bound, err:warnings)
                  Right True  -> return (scope, Skip, bound, warnings)
                  Right False ->  return (scope, stmt, bound, warnings)
              Left errvar ->
                let errmsg = show name ++ " used with invalid variable " ++ show errvar in
                  return (scope, stmt, bound, errmsg:warnings)
          Nothing ->
            -- The deallocated variable is invalidated so the information cannot be used
            -- further on
            let errmsg = show name ++ " has been invalidated" in
              return (scope, stmt, bound, errmsg:warnings)
      else
        return (scope, stmt, bound, warnings)

    Mod (Moderator (Var _ name _ _) op val) _ _ -> do
      -- Moderation can only happen for integer types. This should be checked by a type checker
      (scope', stmt', warnings') <- mod stmt scope warnings
      return (scope', stmt', bound, warnings')

    Mod (Moderator var@(Arr t name sz vals _) op val) idx _ ->
      case tryGetVar name scope of
        Just (arrz3, arr) ->
          processExpr scope val >>= \case
            Right incrVal -> do
              idx' <- calculateIndex scope (fromJust idx) (fromJust sz)
              oldVal <- Z3.mkSelect arrz3 idx'
              newVal <-
                (case op of
                   PlusEq -> Z3.mkBvadd oldVal incrVal
                   SubEq  -> Z3.mkBvsub oldVal incrVal
                   XorEq  -> Z3.mkBvxor oldVal incrVal)
              newVar <- makeArr (fromJust t) name
              newVal' <- Z3.mkStore arrz3 idx' newVal
              Z3.assert =<< Z3.mkEq newVar newVal'
              return (Map.insert name (newVar, var) scope, stmt, bound, warnings)

            Left errvar -> do
              let errmsg = show name ++ " just got invalidated because of " ++ show errvar
              scope' <- invalidateVar scope name
              return (scope', stmt, bound, errmsg:warnings)
        Nothing ->
          return (scope, stmt, bound, (show name ++ " has been invalidated") : warnings)

    Switch (Var (Just t1) n1 _ _) (Var (Just t2) n2 _ _) _ -> do
      newVar1 <- makeVar t1 n1
      newVar2 <- makeVar t2 n2

      case (tryGetVar n1 scope, tryGetVar n2 scope) of
        (Just (n1', v1), Just (n2', v2)) -> do
          Z3.assert =<< Z3.mkEq newVar1 n2'
          Z3.assert =<< Z3.mkEq newVar2 n1'
          return (Map.insert n1 (newVar1, v1) $ Map.insert n2 (newVar2, v2) scope, stmt,
                  bound, warnings)

        _ ->
          -- One of the switched variables has been invalidated
          let errmsg = "Either " ++ show n1 ++ " or " ++ show n2 ++ " has been invalidated" in
            return (scope, stmt, bound, errmsg:warnings)

    Switch (Arr (Just t1) n1 _ _ _) (Arr (Just t2) n2 _ _ _) _ -> do
      newVar1 <- makeVar t1 n1
      newVar2 <- makeVar t2 n2

      case (tryGetVar n1 scope, tryGetVar n2 scope) of
        (Just (n1', v1), Just (n2', v2)) -> do
          Z3.assert =<< Z3.mkEq newVar1 n2'
          Z3.assert =<< Z3.mkEq newVar2 n1'
          return (Map.insert n1 (newVar1, v1) $ Map.insert n2 (newVar2, v2) scope, stmt,
                  bound, warnings)

        _ ->
          let errmsg = "Either " ++ show n1 ++ " or " ++ show n2 ++ " has been invalidated" in
            return (scope, stmt, bound, errmsg:warnings)

    Ite ifcond body1 ficond body2 pos ->
      {-
        1. validate if body
        2. validate fi cond
        3. validate else body
        4. make ITE continuation
      -}
      processExpr scope ifcond >>= \case
        Right ifcond' ->
          if (doOpt) then do
            -- if part
            Z3.push
            prepareAssert scope ifcond >>= \case
              Right scope' -> do
                (body1', scope'', _, _, _) <-
                  processStatements body1 doOpt ast scope' bound 0 []
                processExpr scope'' ficond >>= \case
                  Right ficond' -> do
                    -- fi part
                    fiValidated <- validate ficond ficond'

                    Z3.pop 1

                    -- else part
                    Z3.push
                    Z3.assert =<< Z3.mkNot ifcond' -- TODO: make this use prepareAssert
                    processStatements body2 doOpt ast scope bound 0 []
                    Z3.pop 1

                    -- Need to create the scope of body1 and body2 again, because pop erased them
                    -- TODO: Making change whole push pop above, to make sure we don't need this
                    (body1', scope1, state1, bound1, warnings') <-
                      processStatements body1 doOpt ast scope 0 bound warnings
                    (body2', scope2, state2, bound2, warnings'') <-
                      processStatements body2 doOpt ast scope 0 bound warnings'

                    let bound' = max bound1 bound2

                    -- Continuation
                    ite <- createITE ifcond' scope scope1 scope2

                    case fiValidated of
                      Left err    ->
                        return (ite, Ite ifcond body1' ficond body2' pos, bound', err:warnings'')
                      Right True  ->
                        return (ite, Ite ifcond body1' SkipE body2' pos, bound', warnings'')
                      Right False ->
                        return (ite, Ite ifcond body1' ficond body2' pos, bound', warnings'')

                  Left errvar -> do
                    -- Cannot guarantee anything as the fi conditional uses invalidated variables
                    -- Must invalidate any variables modified within both bodies
                    Z3.pop 1
                    scope' <- invalidateVars scope (nub $ modifiedVars ast body1 ++ modifiedVars ast body2)
                    return (scope',
                            Ite ifcond body1' ficond body2 pos, bound,
                            (show errvar ++ " invalidated if-else statement") : warnings)
              Left errvar -> do
                Z3.pop 1
                scope' <- invalidateVars scope (nub $ modifiedVars ast body1 ++ modifiedVars ast body2)
                return (scope',
                       stmt,
                       bound,
                       (show errvar ++ " invalidated if-else statement") : warnings)
          else do
            (body1', scope1, state1, bound1, warnings') <-
              processStatements body1 doOpt ast scope 0 bound warnings
            (body2', scope2, state2, bound2, warnings'') <-
              processStatements body2 doOpt ast scope 0 bound warnings'
            ite <- createITE ifcond' scope scope1 scope2
            return (ite, stmt, max bound1 bound2, warnings'')

        Left errvar -> do
          -- Cannot guarantee anything as the if conditional uses invalidated variables
          -- Must invalidate any variables modified within both bodies
          scope' <- invalidateVars scope (nub $ modifiedVars ast body1 ++ modifiedVars ast body2)
          return (scope', stmt, bound,
                  (show errvar ++ " invalidated if-else statement") : warnings)

    Assert e _ ->
      processExpr scope e >>= \case
        Right cond ->
          if doOpt
            then do
              validated <- validate stmt cond
              case validated of
                Left err    ->
                  return (scope, stmt, bound, err:warnings)
                Right True  ->
                  Z3.assert cond
                  >> return (scope, Skip, bound, warnings)
                Right False ->
                  Z3.assert cond
                  >> return (scope, stmt, bound, warnings)
            else do
              sat <- satisfiable cond
              if sat
                then
                  Z3.assert cond
                  >> return (scope, stmt, bound, warnings)
                else
                  return (scope, stmt, bound, (show stmt ++ " unsatisfiable"):warnings)
        Left errvar ->
          return (scope, stmt, bound, (show stmt ++ " used invalidated var") : warnings)

    {-
      1. Rename formal parameters and local variables in called procedure.
      2. Connect FArg with AArg.
      3. Process the damn body,
      4. Connect parameters back again
    -}
    Call name aargs pos ->
      processFunctionCall scope aargs (findProc name ast) state bound warnings

    Uncall name aargs pos ->
      let ast' = reverseProcedure (findProc name ast) [] in
        processFunctionCall scope aargs (head ast') state bound warnings


    {-
      1. Check if it has a loop invariant.
      2. if yes then opt with this else check for const for
      3. if yes then opt with unroll else stop

      Loop invariant must be true at entry and exit of each iteration.

      Rules for unrolling:
        No variables used at:
          * Instantiation,
          * Increment,
          * End condition.
    -}
    For1 inv var body m cond b pos -> do
      (scope', b', body', bound', warnings') <-
        processForLoop scope True inv var body m cond pos warnings
      return (scope', For1 inv var body' m cond b' pos, bound', warnings')

    For2 inv var m body cond b pos -> do
      (scope', b', body', bound', warnings') <-
        processForLoop scope False inv var body m cond pos warnings
      return (scope', For2 inv var m body' cond b' pos, bound', warnings')

    Skip -> return (scope, Skip, bound, warnings)
    _ -> error $ "Optimization statement: Stmt not implemented yet - " ++ show stmt
  where
    -- If loop is not unrollable variables modified within loop body, and not asserted in
    -- loop invariant, will be invalidated, as we can no longer use any assertions on these.
    -- Returns (new scope, outer loop optimizable?, optimized body).
    processForLoop :: Vars -> Bool -> Maybe Invariant -> Var -> [Stmt] -> Moderator -> Expr -> Pos ->
      [String] -> Z3 (Vars, LoopInfo, [Stmt], Bound, [String])
    processForLoop scope forward inv var@(Var t name (Just val) p1) body m@(Moderator v op incr) cond
     pos warnings =
      case fromJust t of
        IntegerT ->
          case op of
            XorEq -> error "Xor for-loops not implemented yet"
            _ -> do
              -- creating initial loop variable
              z3var <- makeVar (fromJust t) name
              let scope' = Map.insert name (z3var, var) scope
              processExpr scope val >>= \case
                Right z3expr -> do
                  eq <- Z3.mkEq z3var z3expr
                  Z3.assert eq

                  -- determine loop optimizer method
                  let unrollable =
                        varFreeExprs [val, cond, incr]
                        && modificationFreeIncr name body

                  case (inv, unrollable) of
                    (Just (Invariant inv' _), True) -> do
                      let start = fromRight (-1) (evalConstantIntExpr val)
                      let end   = fromRight (-1) (evalConstantIntExpr cond)
                      let incr' = fromRight (-1) (evalConstantIntExpr incr)
                      (_, inf', _, warnings') <- invariant inv' body z3var scope' warnings

                      if start == -1 || end == -1 || incr' == -1
                        then error "bum"
                        else do let bound' =
                                      bound *
                                      (case op of
                                         PlusEq ->
                                           if (start > end) then
                                             error "Loop utilizing overflow not allowed"
                                           else (end - start) `div` incr'
                                         SubEq ->
                                           if (start < end) then
                                             error "Loop utilizing underflow not allowed"
                                           else (start - end) `div` incr')

                                (body', scope'', validated, warnings'') <-
                                  unroll bound' doOpt forward body pos var z3expr m scope'
                                    state warnings'

                                return (scope''
                                        , LoopInfo validated (getLoopInfoInv inf')
                                        , body'
                                        , bound'
                                        , warnings'')

                                -- if bound' <= unrollBound
                                --   then do 
                                --     -- (body', scope'', state', warnings'') <-
                                --     --   loopBody True forward scope' body state bound' warnings'
                                --     -- bound-1 as we just processed the body once above
                                --     (body', scope'', validated, warnings'') <-
                                --       unroll bound' doOpt forward body pos var z3expr m scope'
                                --         state warnings'

                                --     return (scope''
                                --             , LoopInfo validated (getLoopInfoInv inf')
                                --             , body'
                                --             , bound'
                                --             , warnings'')
                                --   else do
                                --     (scope'', body', validated, warnings'') <-
                                --       generalizedAnalysis scope' body z3expr m state warnings'

                                --     return (  scope''
                                --             , LoopInfo validated (getLoopInfoInv inf')
                                --             , body'
                                --             , bound'
                                --             , "Loop unroll exceeded bound":warnings'')

                    (Just (Invariant inv' _), False) -> do
                      Z3.push
                      (_, body', validated, warnings') <-
                        generalizedAnalysis scope' body z3expr m state warnings
                      Z3.pop 1

                      (scope'', info', body'', warnings'') <-
                        invariant inv' body' z3var scope' warnings'

                      return (scope''
                             , LoopInfo validated (getLoopInfoInv info')
                             , body'', bound, warnings'')

                    (Nothing, True) -> do
                      let start = fromRight (-1) (evalConstantIntExpr val)
                      let end   = fromRight (-1) (evalConstantIntExpr cond)
                      let incr' = fromRight (-1) (evalConstantIntExpr incr)
                      if start == -1 || end == -1 || incr' == -1
                        then error "bam"
                        else do let bound' =
                                      bound *
                                      (case op of
                                          PlusEq ->
                                            if start > end then
                                              error "Loops utilizing overflow is not allowed"
                                            else
                                              (end - start) `div` incr'
                                          SubEq ->
                                            if start < end then
                                              error "Loops utilizing underflow not allowed"
                                            else
                                              (start - end) `div` incr')

                                (body', scope'', validated', warnings') <-
                                  unroll bound' doOpt forward body pos var z3expr m scope' state warnings

                                return (scope''
                                        , LoopInfo validated' invariantStart
                                        , body'
                                        , bound'
                                        , warnings')

                                -- if bound' <= unrollBound
                                --   then do
                                --     -- (body', scope'', state', warnings') <-
                                --     --   loopBody True forward scope' body state bound' warnings

                                --     -- bound-1 as we just processed first iteration above.
                                --     (body', scope'', validated', warnings') <-
                                --       unroll bound' doOpt forward body pos var z3expr m scope' state warnings

                                --     return (scope''
                                --             , LoopInfo validated' invariantStart
                                --             , body'
                                --             , bound'
                                --             , warnings')
                                --   else do
                                --     (scope'', body', validated, warnings') <-
                                --       generalizedAnalysis scope' body z3expr m state warnings

                                --     return (  scope''
                                --             , LoopInfo validated invariantStart
                                --             , body'
                                --             , bound'
                                --             , ("Loop Unroll exceeds bound"):warnings')

                    _ -> do
                      (tmpScope, body', validated, warnings') <-
                        generalizedAnalysis scope' body z3expr m state warnings

                      return ( tmpScope
                             , LoopInfo validated invariantStart
                             , body'
                             , bound
                             , warnings')

                Left errvar ->
                  let errmsg = show name ++ " used invalid variable" in
                    invalidateVars scope (modifiedVars ast body)
                      >>= (\scope' -> return (scope'
                                             , LoopInfo False invariantStart
                                             , body
                                             , bound
                                             , errmsg:warnings))
        BooleanT -> error "Boolean for-loops not implemented yet"
      where
        generalizedAnalysis :: Vars -> [Stmt] -> AST -> Moderator -> Int -> [String] -> Z3 (Vars, [Stmt], Bool, [String])
        generalizedAnalysis scope body z3expr m state warnings = do
          -- Z3.push
          -- Generalize all variables in scope, and check for validity
          tmpScope <- invalidateVars scope $ modifiedVars ast body

          -- Trying to get more information about the loop variable
          let loopVar = fst $ fromJust $ tryGetVar name tmpScope
          loopDirection <-
            loopDescending scope forward var m body state bound warnings
          (case loopDirection of
            Ascending -> Z3.assert =<< Z3.mkBvuge loopVar z3expr
            Descending -> Z3.assert =<< Z3.mkBvule loopVar z3expr
            Unknown -> return ())

          -- Trying to optimize loop based on generalized information
          -- Z3.push
          -- (body', scope'', state', bound', warnings') <-
          --   processStatements body doOpt ast tmpScope state bound warnings
          -- Z3.pop 1

          (body', _, validated, warnings') <-
            unroll 1 doOpt forward body pos var z3expr m tmpScope state warnings
          -- Z3.pop 1
          return (tmpScope, body', validated, warnings')



        varFreeExprs :: [Expr] -> Bool
        varFreeExprs = \case
          []            -> True
          (ConstE _:tl)      -> True && varFreeExprs tl
          (VarE _:tl)        -> False && varFreeExprs tl
          (Arith _ e1 e2:tl) -> varFreeExprs [e1, e2] && varFreeExprs tl
          (Not e:tl)         -> varFreeExprs [e] && varFreeExprs tl
          (Size n _:tl)      -> False -- && varFreeExprs tl
          (SkipE:tl)         -> True && varFreeExprs tl

        modificationFreeIncr :: Ident -> [Stmt] -> Bool
        modificationFreeIncr name1 = \case
          [] -> True
          (Mod (Moderator (Var _ name2 _ _) _ _) _ _:tl) ->
            if (name1 == name2) then False else modificationFreeIncr name1 tl
          (_:tl) -> modificationFreeIncr name1 tl

        {-
            Must prove invariant at:
              1. Initialization of loop.
              2. Maintenance - if true before an iteration also true after.
              3. Termination of loop! Otherwise the whole analysis does not matter.
        -}
        invariant :: Expr -> [Stmt] -> AST -> Vars -> [String] ->
          Z3 (Vars, LoopInfo, [Stmt], [String])
        invariant inv body z3var loopScope warnings'  =
          processExpr loopScope inv >>= \case
            Right z3inv -> do
              validated <- validate stmt z3inv
              case validated of
                Right True -> do
                  -- invariant true at initialization
                  Z3.push
                  loopScope' <- invalidateVars loopScope $ modifiedVars ast body
                  Z3.assert z3inv

                  -- end ite on return () to make both branches have same return type
                  (_, scope, _, _, _) <- loopBody False forward loopScope' body state bound warnings

                  validated <- validate stmt z3inv
                  Z3.pop 1
                  case validated of
                    Right True -> do
                      {-
                        invariant true at initialization+maintenance.
                        This ALWAYS assumes that the decreasing/increasing variable is the
                        loop variable

                        Based on whether <id> starts higher or lower than its end value,
                        I can determine whether it's increasing or decreasing.
                        Now all that is needed is:
                          iteration start
                          (get value of <id>)
                          loop body
                          (assert <id> being either larger or less than start)
                      -}
                      Z3.push
                      -- Trying to get more information about the loop variable
                      let loopVar = fst $ fromJust $ tryGetVar name loopScope'
                      loopDirection <-
                        loopDescending loopScope forward var m body state bound warnings
                      Z3.pop 1

                      case loopDirection of
                        Unknown ->
                          let errmsg = "Loop invariant untrue at termination: " ++ show pos in
                            invalidateVars loopScope (modifiedVars ast body)
                            >>= (\scope'->
                                    return (scope',LoopInfo False 4, body, errmsg:warnings'))
                        _ ->
                          -- Now we know it is terminating, and as it is true during maintenance
                          -- It is also true at termination.
                          -- So create scope, invalidate
                          -- changed variables not part of invariant.
                          -- Optimizing the assertion away is not possible only with invariant.
                          Z3.assert z3inv -- invariant true so carry the information onwards
                          >> invalidateVars loopScope (modifiedVars ast body \\ exprVars inv)
                          >>= (\scope' ->
                                  return (scope', LoopInfo False 0, body, warnings'))
                    _ ->
                      invalidateVars loopScope (modifiedVars ast body)
                      >>= (\scope' -> return (scope', LoopInfo False 6, body,
                            ("Loop invariant not true at maintenance " ++ show pos) : warnings'))
                Right False ->
                  invalidateVars loopScope (modifiedVars ast body)
                  >>= (\scope' -> return (scope', LoopInfo False invariantStart, body,
                        ("Loop invariant not true at initialization " ++ show pos) : warnings'))
                Left errmsg ->
                  invalidateVars loopScope (modifiedVars ast body)
                  >>= (\scope' ->
                         return (scope', LoopInfo False invariantStart, body, errmsg:warnings'))
            Left errvar ->
              invalidateVars loopScope (modifiedVars ast body)
                >>= (\scope' -> return (scope', LoopInfo False invariantStart, body,
                        ("Loop invariant used invalidated variables at " ++ show pos) : warnings'))


        unroll :: Int -> Bool -> Bool -> [Stmt] -> Pos -> Var -> AST -> Moderator -> Vars -> Int ->
          [String] -> Z3 ([Stmt], Vars, Bool, [String])
        unroll bound doOpt forward body pos var@(Var t name _ _) val m scope state warnings =
          case bound of
            x | x <= 0 -> return (body, scope, True, warnings)
            _ ->
              if bound <= unrollBound
                then do
                  (body', scope'', state', bound', warnings') <-
                    loopBody doOpt forward scope body state bound warnings
                  -- checking assertion at the end of loop iteration
                  case tryGetVar name scope'' of
                    Just (z3var, _) ->  do
                      -- Because the loop variable cannot be bogus, we do not need
                      -- to use validate, as there is no need in checking whether
                      -- the expression can be satisfied first.
                      z3ast <- Z3.mkEq z3var val
                      -- Checking satisfiability of negated assert
                      ------------------------------------
                      -- odin <- Z3.solverToString
                      -- trace ("\nodin\n"++show odin++"\n"++show var++"\n") $ return ()
                      ------------------------------------
                      validated <- satisfiable z3ast
                      
                      if not validated
                        then
                          if bound' <= unrollBound
                            then
                              -- Only optimize body on first run
                              unroll (bound-1) False forward body' pos var val m scope'' state' warnings'
                            else 
                              return (body', scope'', True, warnings')
                        else
                          return (body', scope'', False, warnings')

                      -- z3ast     <- Z3.mkNot =<< Z3.mkEq z3var val
                      -- validated <- validate var z3ast
                      ------------------------------------
                      -- odin <- Z3.solverToString
                      -- trace ("\nodin\n"++show odin++"\n"++show var++"\n") $ return ()
                      ------------------------------------
                      -- case validated of
                      --   Right True  ->
                      --     if bound' <= unrollBound
                      --       then
                      --         -- Only optimize body on first run
                      --         unroll (bound-1) False forward z3expr body' pos var val m scope'' state' warnings'
                      --       else 
                      --         return (body', scope'', True, warnings')
                      --   Right False ->
                      --     return (body', scope'', False, warnings')
                      --   Left err ->
                      --     return (body', scope'', False, err:warnings')

                    _ ->
                      let errmsg = "Loop variable " ++ show name ++ " invalidated" in
                        return (body, scope, False, errmsg:warnings')
                else do
                  (scope', body', validated, warnings') <-
                    generalizedAnalysis scope body val m state warnings

                  return (  body'
                          , scope'
                          , validated
                          , ("Loop Unroll exceeds bound"):warnings')


        loopBody :: Bool -> Bool -> Vars -> [Stmt] -> Int -> Bound -> [String] -> Z3 ([Stmt], Vars, Int, Bound, [String])
        loopBody doOpt forward scope body state bound warnings =
          if (forward) then do
            (body', scope', state', bound', warnings') <-
              processStatements body doOpt ast scope state bound warnings
            (scope'', _, warnings'') <- mod (Mod m Nothing pos) scope' warnings'
            return (body', scope'', state', bound', warnings'')
          else do
            (scope', _, warnings') <- mod (Mod m Nothing pos) scope warnings
            (body', scope'', state', bound', warnings'') <-
              processStatements body doOpt ast scope' state bound warnings'
            return (body', scope'', state', bound', warnings'')

        -- | Determines whether given loop is descending or ascending
        loopDescending ::  Vars -> Bool -> Var -> Moderator -> [Stmt] -> Int -> Bound -> [String] -> Z3 Direction
        loopDescending scope forward loopVar@(Var _ n _ _) (Moderator _ op _) body state bound warnings = do
          Z3.push
          -- Setting up loop variable
          let loopVar' = fst $ fromJust $ tryGetVar n scope

          -- Processing loop body
          (_, scope', _, _, _) <-
            loopBody False forward scope body state bound warnings

          -- Initial guess at direction, as it's expensive to prove direction
          -- let newVar = fst $ fromJust $ tryGetVar n scope
          -- isDesc <- isDescending loopVar' newVar
          -- isAsc  <- isAscending loopVar' newVar
          -- case op of
          --   PlusEq ->
          --     if isAsc
          --       then Z3.pop 1 >> return Ascending
          --       else if isDesc
          --         then Z3.pop 1 >> return Descending
          --         else Z3.pop 1 >> return Unknown

          --   SubEq ->
          --     if isDesc
          --       then Z3.pop 1 >> return Descending
          --       else if isAsc
          --         then Z3.pop 1 >> return Ascending
          --         else Z3.pop 1 >> return Unknown
          --   _ -> error "XOR loop not implemented yet!"

          -- Determining whether loop is decreasing
          let newVar = fst $ fromJust $ tryGetVar n scope'
          assDesc <- Z3.mkBvuge loopVar' newVar
          assDescNeg <- Z3.mkNot assDesc
          -- Checking whether the negated is satisfiable
          descending <- satisfiable assDescNeg

          if not descending
            then
              Z3.pop 1 >> return Descending
            else do
              -- It cannot be descending, so check if it's ascending
              -- Have to check whether it's always ascending
              assAsc <- Z3.mkBvule loopVar' newVar
              assAscNeg <- Z3.mkNot assAsc
              ascending <- satisfiable assAscNeg

              -- ascending <- validate "av" assAsc

              Z3.pop 1

              if not ascending
                then return Ascending
                else return Unknown
          -- where
          --   isDescending :: AST -> AST -> Z3 Bool
          --   isDescending oldVar newVar = do
          --     -- Determining whether loop is decreasing
          --     assDesc <- Z3.mkBvuge oldVar newVar
          --     assDescNeg <- Z3.mkNot assDesc
          --     -- Checking whether the negated is satisfiable
          --     descending <- satisfiable assDescNeg
          --     return $ not descending
          --   isAscending :: AST -> AST -> Z3 Bool
          --   isAscending oldVar newVar = do
          --     -- It cannot be descending, so check if it's ascending
          --     -- Have to check whether it's always ascending
          --     assAsc <- Z3.mkBvule oldVar newVar
          --     assAscNeg <- Z3.mkNot assAsc
          --     ascending <- satisfiable assAscNeg
          --     return $ not ascending
              -- case ascending of
              --   Right True -> return Ascending
              --   _ -> return Unknown

          -- descending <- validate loopVar assDesc

          -- case descending of
          --   Right True -> Z3.pop 1 >> return Descending
          --   Right False ->
          --     -- It is possible that it is descending, but also ascending
          --     Z3.pop 1 >> return Unknown

          --   Left err -> do
          --     -- It cannot be descending, so check if it's ascending
          --     -- Have to check whether it's always ascending
          --     assAsc <- Z3.mkBvule loopVar' newVar
          --     ascending <- validate "av" assAsc

          --     Z3.pop 1

          --     case ascending of
          --       Right True -> return Ascending
          --       _ -> return Unknown

    processFunctionCall :: Vars -> [AArg] -> ProcDecl -> Int -> Bound -> [String] ->
      Z3 (Vars, Stmt, Bound, [String])
    processFunctionCall scope aargs p state bound warnings =  do
      let p' = renameProc Map.empty p state
      case p' of
        (vtable, ProcDecl _ fargs' body' _, _) -> do
          scope'  <- foldM connectArgument scope (zip aargs fargs')
          (_, scope'', _, bound', warnings') <-
            processStatements body' False ast scope' 0 bound warnings
          scope'' <- foldM connectArgumentsBack scope'' (zip aargs fargs')
          return $ (scope'', stmt, bound', warnings)

    findProc :: Ident -> [ProcDecl] -> ProcDecl
    findProc n procs =
      case find (\(ProcDecl n' _ _ _) -> n' == n) procs of
        Just p -> p
        Nothing -> error "Could not find function to inline"

    connectArgument :: Vars -> (AArg, FArg) -> Z3 Vars
    connectArgument scope (aargs, fargs) =
      case fargs of
        VarFA t name _ -> do
          newVar <- makeVar t name
          case aargs of
            VarAA var@(Var _ namea _ _) _ _ -> do
              case tryGetVar namea scope of
                Just (varz3, _) -> do
                  Z3.assert =<< Z3.mkEq newVar varz3
                  return $ Map.insert name (newVar, var) scope

                _ -> return scope

        ArrFA t name sz _ -> do
          newVar <- makeArr t name
          case aargs of
            VarAA var@(Arr _ namea _ _ _) _ _ -> do
              case tryGetVar namea scope of
                Just (varz3, _) -> do
                  Z3.assert =<< Z3.mkEq newVar varz3
                  return $ Map.insert name (newVar, var) scope

                _ -> return scope

        ConstFA t name _ -> do
          newVar <- makeVar t name
          case aargs of
            ConstAA (IntegerV val) p -> do
              z3Val <- Z3.mkBvNum intSz val
              Z3.assert =<< Z3.mkEq newVar z3Val
              return $ Map.insert name
                         (newVar, Var (Just t) name (Just (ConstE (IntegerV val))) p) scope

            _ ->
              error $ "Connecting Arguments: Actual argument "
                ++ show (aargs) ++ " does not match constant " ++ show fargs

    connectArgumentsBack :: Vars -> (AArg, FArg) -> Z3 Vars
    connectArgumentsBack scope (VarAA (Var _ name1 _ _) Nothing _, VarFA t name _) = do
      newVar <- makeVar t name1
      case tryGetVar name scope of
        Just (oldVar, var) -> do
          Z3.assert =<< Z3.mkEq newVar oldVar
          return $ Map.insert name1 (newVar, var) scope

        _ -> return scope

    connectArgumentsBack scope (VarAA (Arr _ name1 _ _ _) (Just idxs) _, ArrFA t name _ _) = do
      newVar <- makeArr t name1
      case tryGetVar name scope of
        Just (oldVar, var) -> do
          Z3.assert =<< Z3.mkEq newVar oldVar
          return $ Map.insert name1 (newVar, var) scope

        _ -> return scope

    connectArgumentsBack scope _ = return scope


processStatements :: [Stmt] -> Bool -> [ProcDecl] -> Vars -> Int -> Bound -> [String] ->
  Z3 ([Stmt], Vars, Int, Bound, [String])
processStatements body doOpt ast scope state bound warnings =
  foldM (f ast) ([], scope, state, bound, warnings) body
    >>= (\(body', scope', state', bound', warnings') ->
           return (reverse body', scope', state', bound', warnings'))
  where
    f :: [ProcDecl] -> ([Stmt], Vars, Int, Bound, [String]) -> Stmt ->
      Z3 ([Stmt], Vars, Int, Bound, [String])
    f ast (acc, scope, state, bound, warnings) stmt = do
      (scope', stmt', bound', warnings') <-
        processStatement doOpt ast scope stmt state bound warnings
      return (stmt':acc, scope', state + 1, bound', warnings')


processArgs :: [FArg] -> Vars -> Z3 Vars
processArgs args scope = foldM f scope args
  where
    f :: Vars -> FArg -> Z3 Vars
    f vars arg =
      case arg of
        VarFA t n p -> do
          newVar <- makeVar t n
          let var = Var (Just t) n Nothing p
          return $ Map.insert n (newVar, var) vars
        ArrFA t n s p -> do
          newArr <- makeArr t n
          let var = Arr (Just t) n (Just s) Nothing p
          return $ Map.insert n (newArr, var) vars
        ConstFA t n p -> do
          newVar <- makeVar t n
          let var = Var (Just t) n Nothing p
          return $ Map.insert n (newVar, var) vars


processProcedure :: [ProcDecl] -> Vars -> ([ProcDecl], [String]) -> ProcDecl ->
  Z3 ([ProcDecl], [String])
processProcedure ast scope (accp, warnings) (ProcDecl name args body pos) = do
  -- Make sure assertions created for one procedure does not influence another
  Z3.push
  initialScope <- processArgs args scope
  (body', _, _, _, newWarnings) <- processStatements body True ast initialScope 0 1 warnings
  Z3.pop 1

  return (ProcDecl name args body' pos : accp, newWarnings)


processMainStore :: ProcDecl -> Z3 Vars
processMainStore (ProcDecl name _ body _) =
  case name of
    Ident "main" _ -> foldM f Map.empty body
    _ -> error "This was no main"
  where
    f :: Vars -> Stmt -> Z3 Vars
    f scope = \case
      Global var@(Var t name val _) _    -> do
        newVar <- makeVar (fromJust t) name
        return $ Map.insert name (newVar, var) scope
      Global var@(Arr t name sz val _) _ -> do
        newVar <- makeArr (fromJust t) name
        return $ Map.insert name (newVar, var) scope
      _ ->
        return scope


-- First decl in decls is the main function.
processProgram :: Program -> Bool -> Z3 (Program, [String])
processProgram (Program decls) doMain = do
  initialScope <- processMainStore $ head decls

  -- Backwards optimization should ignore the main function
  let decls' = if doMain then decls else tail decls

  (odecls, warnings) <- foldM (processProcedure (tail decls) initialScope) ([], []) decls'

  -- Need to attach the main function again
  let odecls' = if doMain then reverse odecls else head decls :(reverse odecls)

  return (Program $ odecls', reverse warnings)
