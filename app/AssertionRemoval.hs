{-# LANGUAGE LambdaCase #-}
module AssertionRemoval where

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

type Vars = Map Ident (AST, Var)

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


-- If Nothing is returned the array being set should be invalidated
setArray :: Vars -> [Expr] -> Int -> AST -> Z3 (Either Ident AST)
setArray scope vals idx arr =
  case vals of
    [] -> return $ Right arr
    (hd:tl) -> do
      idx' <- Z3.mkInt idx =<< Z3.mkBvSort intSz
      hd'  <- processExpr scope hd
      case hd' of
        Right x ->
          Z3.mkStore arr idx' x >>= (setArray scope tl (idx+1))
        otherwise -> return otherwise



-- If this reports Nothing during processing it's because name is invalidated.
-- If it happens during renaming, it's because not all variables need to be renamed
tryGetVar :: Ident -> Map Ident a -> Maybe a
tryGetVar name scope =
  Map.lookup name scope


invalidateVars :: [Ident] -> Vars -> Z3 Vars
invalidateVars stmts scope =
  foldM (\s key -> return $ Map.delete key s) scope stmts

modifiedVars :: [Stmt] -> [Ident]
modifiedVars = \case
  [] -> []

  (Mod (Moderator (Var _ name _ _) _ _) _ _:tl)   -> name : modifiedVars tl
  (Mod (Moderator (Arr _ name _ _ _) _ _) _ _:tl) -> name : modifiedVars tl

  (Switch (Var _ name1 _ _) (Var _ name2 _ _) _:tl) -> name1 : name2 : modifiedVars tl
  (Switch (Arr _ name1 _ _ _) (Arr _ name2 _ _ _) _:tl) -> name1 : name2 : modifiedVars tl

  (_:tl) -> modifiedVars tl


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


-- If this reports Nothing the variable using this expression should be invalidated
processExpr :: Vars -> Expr -> Z3 (Either Ident AST)
processExpr scope e =
  case e of
    ConstE v ->
      case v of
        IntegerV i -> Z3.mkBvNum intSz i >>= (\val -> return $ Right val)
        BooleanV b -> Z3.mkBool b >>= (\val -> return $ Right val)

    VarE var ->
      case var of
        LVar n ->
          case tryGetVar n scope of
            Just (n', _) -> return $ Right n'
            Nothing -> return (Left n)

        _ -> error "VarE for array not implemented yet"

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
  where f scope name = do
          -- During this phase it should always be Just, as we work on the origin scope
          let (var1z3, var)   = fromJust $ tryGetVar name scope1
          let (var2z3, _)   = fromJust $ tryGetVar name scope2

          -- Use var from one branch as type should be identical
          newVar    <- makeVar (fromJust (getVarType var)) name
          ite       <- Z3.mkIte cond var1z3 var2z3
          Z3.assert =<< Z3.mkEq newVar ite
          return $ Map.insert name (newVar, var) scope


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
    Z3.Unsat -> return (Left $ "Z3 expression is a fallact: " ++ show stmt)
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
        Left errvar -> error "ahh array val could not be processe during validation"

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

validate :: Stmt -> AST -> Z3 (Either String Bool) --(Bool, String)
validate stmt expr = do
  Z3.push
  Z3.assert expr
  val <- Z3.solverCheck
  case val of
    Z3.Sat -> do
      Z3.pop 1
      Z3.push
      Z3.assert =<< Z3.mkNot expr
      val' <- Z3.solverCheck
      Z3.pop 1
      case val' of
        Z3.Sat -> return $ Right False
        Z3.Unsat -> return $ Right True

    Z3.Unsat -> Z3.pop 1 >> return (Left $ "Z3 expression is a fallacy: " ++ show stmt)


-- Processes stmt in given ast and scope, and returns (updated scope, optimized stmt)
-- Try optimize when:
--    * dealloc is met
--    * ite is met
--    * for loop is met
processStatement :: Bool -> [ProcDecl] -> Vars -> Stmt -> Int -> [String] ->
  Z3 (Vars, Stmt, [String])
processStatement doOpt ast scope stmt state warnings =
  trace (show stmt) $ case stmt of
    Global var@(Var t name val _) _ -> do
      newVar <- makeVar (fromJust t) name

      processExpr scope (fromJust val) >>= \case
        Right newVal -> do
          Z3.assert =<< Z3.mkEq newVar newVal
          return (Map.insert name (newVar, var) scope, stmt, warnings)

        Left errvar ->
          let errmsg = show name ++ " used with invalid variable " ++ show errvar in
            return (scope, stmt, errmsg:warnings)

    Global var@(Arr t name sz vals _) _ -> do
      newVar  <- makeArr (fromJust t) name
      setArray scope (fromJust vals) 0 newVar >>= \case
        Right newVar' ->
          return (Map.insert name (newVar', var) scope, stmt, warnings)

        Left errvar ->
          let errmsg = show name ++ " used with invalid variable " ++ show errvar in
            return (scope, stmt, errmsg:warnings)

    Local var@(Var t name val _) _ -> do
      newVar <- makeVar (fromJust t) name

      processExpr scope (fromJust val) >>= \case
        Right newVal -> do
          Z3.assert =<< Z3.mkEq newVar newVal
          return (Map.insert name (newVar, var) scope, stmt, warnings)

        Left errvar ->
          let errmsg = show name ++ " used with invalid variable " ++ show errvar in
            return (scope, stmt, errmsg:warnings)

    Local var@(Arr t name sz vals _) _ -> do
      newVar <- makeArr (fromJust t) name
      setArray scope (fromJust vals) 0 newVar >>= \case
        Right newVar' ->
          return (Map.insert name (newVar', var) scope, stmt, warnings)

        Left errvar ->
          let errmsg = show name ++ " used with invalid variable " ++ show errvar in
            return (scope, stmt, errmsg:warnings)

    DLocal (Var t name val _) _ ->
      if (doOpt) then
        -- val     <- processExpr scope (fromJust val)
        processExpr scope (fromJust val) >>= \case
          Right val ->
            case tryGetVar name scope of
              Just (varz3, var) -> do
                eq <- Z3.mkEq varz3 val
                validated <- validate stmt eq
                case validated of
                  Left err    ->
                    -- Continue without the assertion having any effect on the analysis
                    return (scope, stmt, err:warnings)
                  Right True  -> return (scope, Skip, warnings)
                  Right False -> Z3.assert eq >> return (scope, stmt, warnings)

              Nothing ->
                -- The deallocated variable is invalidated so the information cannot
                -- be used further on
                trace "found" (return (scope, stmt, (show name ++ " has been invalidated") : warnings))

          Left errvar ->
            let errmsg = show name ++ " used with invalid variable " ++ show errvar in
              return (scope, stmt, errmsg:warnings)

      else
        return (scope, stmt, warnings)

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
                    return (scope, stmt, err:warnings)
                  Right True  -> return (scope, Skip, warnings)
                  Right False ->  return (scope, stmt, warnings)
              Left errvar ->
                let errmsg = show name ++ " used with invalid variable " ++ show errvar in
                  return (scope, stmt, errmsg:warnings)
          Nothing ->
            -- The deallocated variable is invalidated so the information cannot be used
            -- further on
            let errmsg = show name ++ " has been invalidated" in
              return (scope, stmt, errmsg:warnings)
      else
        return (scope, stmt, warnings)

    Mod (Moderator (Var _ name _ _) op val) _ _ ->
      -- Moderation can only happen for integer types. This should be checked by a type checker
      mod stmt scope warnings

    Mod (Moderator var@(Arr t name sz vals _) op val) idx _ ->
      case tryGetVar name scope of
        Just (arrz3, arr) ->
          processExpr scope val >>= \case
            Right incrVal -> case arrayIndex (fromJust idx) of
              Right i -> do
                idx' <- trace ("idx " ++ show i) $ Z3.mkBvNum intSz i
                oldVal  <- Z3.mkSelect arrz3 idx'
                newVal  <-
                  case op of
                    PlusEq -> Z3.mkBvadd oldVal incrVal
                    SubEq  -> Z3.mkBvsub oldVal incrVal
                    XorEq  -> Z3.mkBvxor oldVal incrVal
                newVar  <- makeArr (fromJust t) name
                newVal' <- Z3.mkStore arrz3 idx' newVal
                Z3.assert =<< Z3.mkEq newVar newVal'
                return (Map.insert name (newVar, var) scope, stmt, warnings)

              Left err ->
                let errmsg = show name ++ " just got invalidated because of " ++ err in
                  return (Map.delete name scope, stmt, errmsg:warnings)

            Left errvar ->
              let errmsg = show name ++ " just got invalidated because of " ++ show errvar in
              return (Map.delete name scope, stmt, errmsg:warnings)
        Nothing ->
          return (scope, stmt, (show name ++ " has been invalidated") : warnings)

    Switch (Var (Just t1) n1 _ _) (Var (Just t2) n2 _ _) _ -> do
      newVar1 <- makeVar t1 n1
      newVar2 <- makeVar t2 n2

      case (tryGetVar n1 scope, tryGetVar n2 scope) of
        (Just (n1', v1), Just (n2', v2)) -> do
          Z3.assert =<< Z3.mkEq newVar1 n2'
          Z3.assert =<< Z3.mkEq newVar2 n1'
          return (Map.insert n1 (newVar1, v1) $ Map.insert n2 (newVar2, v2) scope, stmt, warnings)

        _ ->
          -- One of the switched variables has been invalidated
          let errmsg = "Either " ++ show n1 ++ " or " ++ show n2 ++ " has been invalidated" in
            return (scope, stmt, errmsg:warnings)

    Switch (Arr (Just t1) n1 _ _ _) (Arr (Just t2) n2 _ _ _) _ -> do
      newVar1 <- makeVar t1 n1
      newVar2 <- makeVar t2 n2

      case (tryGetVar n1 scope, tryGetVar n2 scope) of
        (Just (n1', v1), Just (n2', v2)) -> do
          Z3.assert =<< Z3.mkEq newVar1 n2'
          Z3.assert =<< Z3.mkEq newVar2 n1'
          return (Map.insert n1 (newVar1, v1) $ Map.insert n2 (newVar2, v2) scope, stmt, warnings)

        _ ->
          let errmsg = "Either " ++ show n1 ++ " or " ++ show n2 ++ " has been invalidated" in
            return (scope, stmt, errmsg:warnings)

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
            Z3.assert ifcond'
            -- body1'  <- processStatements doOpt ast scope body1 0
            (_, scope', _, _) <- processStatements body1 doOpt ast scope 0 []
            processExpr scope' ficond >>= \case
              Right ficond' -> do
                -- fi part
                fiValidated <- validate stmt ficond'
                Z3.pop 1

                -- else part
                Z3.push
                Z3.assert =<< Z3.mkNot ifcond'
                -- processStatements doOpt ast scope body2 0
                processStatements body2 doOpt ast scope 0 []
                Z3.pop 1

                -- Need to create the scope of body1 and body2 again, because pop erased them
                -- TODO: Making change whole push pop above, to make sure we don't need this
                (body1', scope1, state1, warnings') <-
                  processStatements body1 doOpt ast scope 0 warnings
                (body2', scope2, state2, warnings'') <-
                  processStatements body2 doOpt ast scope 0 warnings'

                -- Continuation
                ite <- createITE ifcond' scope scope1 scope2

                case fiValidated of
                  Left err    ->
                    return (ite, Ite ifcond body1' ficond body2' pos, err:warnings'')
                  Right True  ->
                    return (ite, Ite ifcond body1' SkipE body2' pos, warnings'')
                  Right False ->
                    return (ite, Ite ifcond body1' ficond body2' pos, warnings'')

              Left errvar -> do
                -- Cannot guarantee anything as the fi conditional uses invalidated variables
                -- Must invalidate any variables modified within both bodies
                scope' <- invalidateVars (nub $ modifiedVars body1 ++ modifiedVars body2) scope
                return (scope', stmt, (show errvar ++ " invalidated if-else statement") : warnings)
          else do
            (body1', scope1, state1, warnings') <-
              processStatements body1 doOpt ast scope 0 warnings
            (body2', scope2, state2, warnings'') <-
              processStatements body2 doOpt ast scope 0 warnings'
            ite <- createITE ifcond' scope scope1 scope2
            return (ite, stmt, warnings'')

        Left errvar -> do
          -- Cannot guarantee anything as the if conditional uses invalidated variables
          -- Must invalidate any variables modified within both bodies
          scope' <- invalidateVars (nub $ modifiedVars body1 ++ modifiedVars body2) scope
          return (scope', stmt, (show errvar ++ " invalidated if-else statement") : warnings)


    Assert e _ ->
      processExpr scope e >>= \case
        Right cond -> do
          validated <- validate stmt cond
          case validated of
            Left err    ->
              return (scope, stmt, err:warnings)
            Right True  ->
              Z3.assert cond
              >> return (scope, if (doOpt) then Skip else stmt, warnings)
            Right False ->
              Z3.assert cond
              >> Z3.assert cond >> return (scope, stmt, warnings)

        Left errvar ->
          return (scope, stmt, (show stmt ++ " used invalidated var") : warnings)

    {-
      1. Rename formal parameters and local variables in called procedure.
      2. Connect FArg with AArg.
      3. Process the damn body,
      4. Connect parameters back again
    -}
    Call name aargs pos ->
      processFunctionCall scope aargs (findProc name ast) state warnings

    Uncall name aargs pos ->
      let ast' = reverseProcedure (findProc name ast) [] in
        processFunctionCall scope aargs (head ast') state warnings


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
      (scope', b', body', warnings') <- processForLoop True inv var body m cond pos warnings
      return (scope', For1 inv var body' m cond b' pos, warnings')

    For2 inv var m body cond b pos -> do
      (scope', b', body', warnings') <- processForLoop False inv var body m cond pos warnings
      return (scope', For2 inv var m body' cond b' pos, warnings')

    Skip -> return (scope, Skip, warnings)
    _ -> error $ "Optimization statement: Stmt not implemented yet - " ++ show stmt
  where
    mod :: Stmt -> Vars -> [String] -> Z3 (Vars, Stmt, [String])
    mod (Mod (Moderator var@(Var t name _ _) op val) _ _) scope warnings = do
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

            Left errvar ->
              let errmsg = show name ++ " invalidated because of " ++ show errvar in
                return (Map.delete name scope, stmt, errmsg:warnings)

        _ ->
          return (scope, stmt, (show name ++ " used invalidated var") : warnings)

    mod _ _ _ = error "Please only use this function for processing modification!"

    -- If loop is not unrollable variables modified within loop body, and not asserted in
    -- loop invariant, will be invalidated, as we can no longer use any assertions on these.
    -- Returns (new scope, outer loop optimizable?, optimized body).
    processForLoop :: Bool -> Maybe Invariant -> Var -> [Stmt] -> Moderator -> Expr -> Pos ->
      [String] -> Z3 (Vars, LoopInfo, [Stmt], [String])
    processForLoop forward inv var@(Var t name (Just val) p1) body m@(Moderator v op incr) cond
     pos warnings =
      case fromJust t of
        IntegerT ->
          case op of
            XorEq -> error "Xor for-loops not implemented yet"
            _ -> do
              -- creating initial loop variable
              z3var <- makeVar (fromJust t) name
              processExpr scope val >>= \case
                Right z3expr -> do
                  eq <- Z3.mkEq z3var z3expr
                  Z3.assert eq
                  let scope' = Map.insert name (z3var, var) scope

                  Z3.push
                  (body', scope'', state', warnings') <-
                    processStatements body doOpt ast scope' state warnings
                  Z3.pop 1

                  -- determine loop optimizer method
                  let unrollable = varFreeExprs [val, cond, incr] && modificationFreeIncr name body
                  case (inv, unrollable) of
                    (Just (Invariant inv' _), True) -> do
                      let start = fromRight (-1) (evalConstantIntExpr val)
                      let end   = fromRight (-1) (evalConstantIntExpr cond)
                      let incr' = fromRight (-1) (evalConstantIntExpr incr)

                      if start == -1 || end == -1 || incr' == -1
                        then error "bum"
                        else return ()

                      (_, inf', _, warnings'') <- invariant inv' body' z3var scope' warnings'

                      if (start > end) then
                        error "Loops utilizing overflow/underflow is not allowed yet"
                      else do
                        let bound = (end - start) `div` incr'
                        (scope'', validated, warnings''') <-
                          unroll bound forward body' pos var z3expr m scope' state warnings''

                        if (validated) then
                          return (scope'', LoopInfo True (getLoopInfoInv inf'), body', warnings''')
                        else
                          return
                            (scope'', LoopInfo False (getLoopInfoInv inf'), body', warnings''')

                    (Just (Invariant inv' _), False) ->
                      invariant inv' body' z3var scope' warnings'

                    (Nothing, True) -> do
                      let start = fromRight (-1) (evalConstantIntExpr val)
                      let end   = fromRight (-1) (evalConstantIntExpr cond)
                      let incr' = fromRight (-1) (evalConstantIntExpr incr)

                      -- optimize using unrolling only
                      if start > end then
                        error "Loops utilizing overflow/underflow is not allowed yet"
                      else do
                        let bound = (end - start) `div` incr'
                        (scope'', validated, warnings'') <-
                          unroll bound forward body' pos var z3expr m scope' state warnings'

                        if (validated) then
                          return (scope'', LoopInfo True 0, body', warnings'')
                        else
                          return (scope'', LoopInfo False 0, body', warnings'')

                    _ ->
                      -- cannot optimize at all :(
                      let errmsg = "Loop unrollable: " ++ show pos in
                        invalidateVars (modifiedVars body) scope
                          >>= (\scope' ->
                                 return (scope', LoopInfo False 0, body', errmsg:warnings))
                Left errvar ->
                  let errmsg = show name ++ " used invalid variable" in
                    invalidateVars (modifiedVars body) scope
                      >>= (\scope' -> return (scope', LoopInfo False 0, body, errmsg:warnings))
        BooleanT -> error "Boolean for-loops not implemented yet"
      where
        varFreeExprs :: [Expr] -> Bool
        varFreeExprs = \case
          []            -> True
          (ConstE _:tl)      -> True && varFreeExprs tl
          (VarE _:tl)        -> False && varFreeExprs tl
          (Arith _ e1 e2:tl) -> varFreeExprs [e1, e2] && varFreeExprs tl
          (Not e:tl)         -> varFreeExprs [e] && varFreeExprs tl
          (Size n _:tl)      -> True && varFreeExprs tl
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
                  Z3.assert z3inv

                  -- end ite on return () to make both branches have same return type
                  (scope) <-
                    if (forward) then do
                      (_, scope'', _, _) <-
                        processStatements body False ast loopScope state warnings
                      (scope''', _, _) <- mod (Mod m Nothing pos) scope'' warnings
                      return scope'''
                    else do
                      (scope'', _, _) <- mod (Mod m Nothing pos) loopScope warnings
                      (_, scope''', _, _) <-
                        processStatements body False ast scope'' state warnings
                      return scope'''

                  validated <- validate stmt z3inv
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
                      case tryGetVar name scope of
                        Just (z3var', _) -> do
                          z3ast <-
                            -- TODO: using the operator might not be good enough e.g. i += -1
                            case op of
                              PlusEq -> Z3.mkBvugt z3var' z3var
                              _      -> Z3.mkBvult z3var' z3var

                          validate stmt z3ast >>= \case
                            Right True ->
                              -- also true at termination. So create scope, invalidate
                              -- changed variables not part of invariant.
                              -- Optimizing the assertion away is not possible only with invariant.
                              Z3.pop 1
                              >> Z3.assert z3inv -- invariant true so carry the information onwards
                              >> invalidateVars (modifiedVars body \\ exprVars inv) scope
                              >>= (\scope' ->
                                     return (scope', LoopInfo False 0, body, warnings'))

                            _ ->
                              let errmsg = "Loop invariant untrue at termination: " ++ show pos in
                                Z3.pop 1
                                >> invalidateVars (modifiedVars body) scope
                                >>= (\scope'->
                                       return (scope',LoopInfo False 4, body, errmsg:warnings'))
                        _ ->
                          Z3.pop 1 >>
                          return (scope, LoopInfo False 4, body,
                            ("Loop variable " ++ show name ++ " has been invalidated") : warnings')
                    _ ->
                      Z3.pop 1
                      >> invalidateVars (modifiedVars body) scope
                      >>= (\scope' -> return (scope', LoopInfo False 6, body,
                            ("Loop invariant not true at maintenance " ++ show pos) : warnings'))
                Right False ->
                  invalidateVars (modifiedVars body) scope
                  >>= (\scope' -> return (scope', LoopInfo False invariantStart, body,
                        ("Loop invariant not true at initialization " ++ show pos) : warnings'))
                Left errmsg ->
                  invalidateVars (modifiedVars body) scope
                  >>= (\scope' ->
                         return (scope', LoopInfo False invariantStart, body, errmsg:warnings'))
            Left errvar ->
              invalidateVars (modifiedVars body) scope
                >>= (\scope' -> return (scope', LoopInfo False invariantStart, body,
                        ("Loop invariant used invalidated variables at " ++ show pos) : warnings'))


        unroll :: Int -> Bool -> [Stmt] -> Pos -> Var -> AST -> Moderator -> Vars -> Int ->
          [String] -> Z3 (Vars, Bool, [String])
        unroll bound forward body pos var@(Var t name _ _) val m scope state warnings =
          case bound of
            0 -> return (scope, True, warnings)
            _ -> do
              (scope'', state', warnings') <-
                if (forward) then do
                  (_, scope', state', _) <- processStatements body False ast scope state []
                  (scope'', _, warnings') <- mod (Mod m Nothing pos) scope' warnings
                  return (scope'', state', warnings')
                else do
                  (scope', _, warnings') <- mod (Mod m Nothing pos) scope warnings
                  (_, scope'', state', _) <- processStatements body False ast scope' state []
                  return (scope'', state', warnings')

              -- checking assertion at the end of loop iteration
              case tryGetVar name scope'' of
                Just (z3var, _) -> do
                  z3ast     <- Z3.mkNot =<< Z3.mkEq z3var val
                  validated <- validate (Local var pos) z3ast
                  case validated of
                    Left err    ->
                      unroll (bound-1) forward body pos var val m scope'' state' (err:warnings')
                    Right True  ->
                      unroll (bound-1) forward body pos var val m scope'' state' warnings'
                    Right False ->
                      error "loop not validating not implemented yet"

                _ ->
                  let errmsg = "Loop variable " ++ show name ++ " invalidated" in
                    return (scope, False, errmsg:warnings)


    processFunctionCall :: Vars -> [AArg] -> ProcDecl -> Int -> [String] ->
      Z3 (Vars, Stmt, [String])
    processFunctionCall scope aargs p state warnings =  do
      p' <- renameProc Map.empty p state
      case p' of
        (vtable, ProcDecl _ fargs' body' _, _) -> do
          scope'  <- foldM connectArgument scope (zip aargs fargs')
          -- closure <- processStatements False ast scope' body' 0
          (_, scope'', _, warnings') <- processStatements (reverse body') False ast scope' 0 warnings
          scope'' <- foldM connectArgumentsBack scope'' (zip aargs fargs')
          return $ (scope'', stmt, warnings)

    findProc :: Ident -> [ProcDecl] -> ProcDecl
    findProc n procs =
      case find (\(ProcDecl n' _ _ _) -> n' == n) procs of
        Just p -> p
        Nothing -> error "Could not find function to inline"

    renameProc :: Map Ident Ident -> ProcDecl -> Int -> Z3 (Map Ident Ident, ProcDecl, Int)
    renameProc vtable (ProcDecl name fargs body pos) state =
      foldM renameArg (vtable, ProcDecl name [] [] pos, state) fargs
        >>= (\acc -> foldM renameStmt acc body)
        >>= (\(vtable, (ProcDecl name args body pos), state) ->
          return (vtable, ProcDecl name (reverse args) (reverse body) pos, state))

      where
        renameArg :: (Map Ident Ident, ProcDecl, Int) -> FArg ->
          Z3 (Map Ident Ident, ProcDecl, Int)
        renameArg (vtable, ProcDecl namep args body posp, state) = \case
          VarFA t n@(Ident name p) pos ->
            let n' = Ident (show state ++ name) p in
              return ( Map.insert n n' vtable
                      , ProcDecl namep (VarFA t n' pos : args) body posp
                      , state + 1 )

          ArrFA t n@(Ident name p) sz pos ->
            let n' = Ident (show state ++ name) p in
              return ( Map.insert n n' vtable
                      , ProcDecl namep (ArrFA t n' sz pos : args) body posp
                      , state + 1)

          ConstFA t n@(Ident name p) pos ->
            let n' = Ident (show state ++ name) p in
              return ( Map.insert n n' vtable
                      , ProcDecl namep (ConstFA t n' pos : args) body posp
                      , state + 1)


        renameExpr :: Map Ident Ident -> Expr -> Expr
        renameExpr vtable = \case
          VarE (LVar name) ->
            let name' = tryGetVar name vtable in
              case name' of
                Just n  -> VarE (LVar n)
                Nothing -> VarE (LVar name)

          VarE (Lookup name exprs) ->
            let name' = tryGetVar name vtable in
              let exprs' = renameExprs vtable exprs in
                case name' of
                  Just n  -> VarE (Lookup n exprs')
                  Nothing -> VarE (Lookup name exprs')

          Arith op expr1 expr2 ->
            let expr1' = renameExpr vtable expr1 in
              let expr2' = renameExpr vtable expr2 in
                Arith op expr1' expr2'

          Not expr ->
            let expr' = renameExpr vtable expr in
              Not expr'

          Size (LVar name) t ->
            let name' = tryGetVar name vtable in
              case name' of
                Just n  -> Size (LVar n) t
                Nothing -> Size (LVar name) t

          Size (Lookup name idxs) t ->
            let name' = tryGetVar name vtable in
              case name' of
                Just n -> Size (Lookup n idxs) t
                Nothing -> Size (Lookup name idxs) t

          otherwise -> otherwise


        renameExprs :: Map Ident Ident -> [Expr] -> [Expr]
        renameExprs vtable = \case
          [] -> []
          (hd:tl) -> renameExpr vtable hd : renameExprs vtable tl


        renameStmt :: (Map Ident Ident, ProcDecl, Int) -> Stmt ->
          Z3 (Map Ident Ident, ProcDecl, Int)
        renameStmt (vtable, p@(ProcDecl namep args body posp), state) = \case
          Local (Var t name val pv) pos -> do
            (vtable', name') <- insert name state vtable
            let val'          = renameExpr vtable' (fromJust val)
            return ( vtable'
                    , ProcDecl namep args (Local (Var t name' (Just val') pv) pos : body) posp
                    , state + 1)

          Local (Arr t name sz vals pv) pos -> do
            (vtable', name') <- insert name state vtable
            let vals'         = renameExprs vtable' (fromJust vals)
            return ( vtable'
                    , ProcDecl namep args (Local (Arr t name' sz (Just vals') pv) pos : body) posp
                    , state + 1)

          DLocal (Var t name val pv) pos -> do
            (vtable', name') <- insert name state vtable
            let val'          = renameExpr vtable' (fromJust val)
            return ( vtable'
                    , ProcDecl namep args (DLocal (Var t name' (Just val') pv) pos : body) posp
                    , state + 1)

          DLocal (Arr t name sz vals pv) pos -> do
            (vtable', name') <- insert name state vtable
            let vals'         = renameExprs vtable' (fromJust vals)
            return ( vtable'
                    , ProcDecl namep args (DLocal (Arr t name' sz (Just vals') pv) pos : body) posp
                    , state + 1)

          Mod (Moderator (Var t name val pv) op expr) _ pos -> do
            let tmp = tryGetVar name vtable
            let name' = if (isJust tmp) then fromJust tmp else name
            let expr' = renameExpr vtable expr
            let mod'  = Moderator (Var t name' val pv) op expr'
            return ( vtable
                    , ProcDecl namep args (Mod mod' Nothing pos : body) posp
                    , state)

          Mod (Moderator (Arr t name sz val pv) op expr) idxs pos -> do
            let tmp = tryGetVar name vtable
            let name' = if (isJust tmp) then fromJust tmp else name
            let expr' = renameExpr vtable expr
            let mod'  = Moderator (Arr t name' sz val pv) op expr'
            let idxs' = renameExprs vtable (fromJust idxs)
            return ( vtable
                    , ProcDecl namep args (Mod mod' (Just idxs') pos : body) posp
                    , state)

          Switch (Var t name val pv) (Var t2 name2 val2 pv2) pos -> do
            let tmp = tryGetVar name vtable
            let name' = if (isJust tmp) then fromJust tmp else name
            let tmp2 = tryGetVar name2 vtable
            let name2' = if (isJust tmp2) then fromJust tmp2 else name2
            return ( vtable
                    , ProcDecl namep args
                        (Switch (Var t name' val pv) (Var t2 name2' val2 pv2) pos : body)
                        posp
                    , state)

          Switch (Arr t name sz val pv) (Arr t2 name2 sz2 val2 pv2) pos -> do
            let tmp = tryGetVar name vtable
            let name' = if (isJust tmp) then fromJust tmp else name
            let tmp2 = tryGetVar name2 vtable
            let name2' = if (isJust tmp2) then fromJust tmp2 else name2
            return ( vtable
                    , ProcDecl namep args
                        (Switch (Arr t name' sz val pv) (Arr t2 name2' sz val2 pv2) pos : body)
                        posp
                    , state)

          Ite condif bodyif condfi bodyelse pos -> do
            let condif' = renameExpr vtable condif
            let condfi' = renameExpr vtable condfi
            tmp        <- foldM renameStmt (vtable, ProcDecl namep [] [] posp, state) bodyif
            case tmp of
              (vtable1, ProcDecl _ _ stmtsif _, state1) -> do
                tmp2 <- foldM renameStmt (vtable, ProcDecl namep [] [] posp, state1) bodyelse
                case tmp2 of
                  (vtable2, ProcDecl _ _ stmtselse _, state2) ->
                    -- Use old vtable as variables introduced in ite is in a tighter scope
                    return ( vtable
                            , ProcDecl namep args
                                (Ite condif' stmtsif condfi' stmtselse pos : body)
                                posp
                            , state2)

          For1 inv (Var t name val pv) body (Moderator (Var t2 _ val2 pv2) op expr) cond b pos -> do
            (vtable', name') <- insert name state vtable
            let val' = renameExpr vtable (fromJust val)
            tmp2 <- foldM renameStmt (vtable', ProcDecl namep [] [] posp, state + 1) body
            case tmp2 of
              (vtablebody, ProcDecl _ _ stmts _, state1) -> do
                let expr' = renameExpr vtable expr
                let cond' = renameExpr vtable' cond
                let mod'  = Moderator (Var t2 name' val2 pv2) op expr'
                let for1' = For1 inv (Var t name' (Just val') pv) stmts mod' cond' b pos
                return ( vtable
                        , ProcDecl namep args (for1':body) posp
                        , state1)

          For2 inv (Var t name val pv) (Moderator (Var t2 _ val2 pv2) op expr) body cond b pos -> do
            (vtable', name') <- insert name state vtable
            let val' = renameExpr vtable (fromJust val)
            tmp2 <- foldM renameStmt (vtable', ProcDecl namep [] [] posp, state + 1) body
            case tmp2 of
              (vtablebody, ProcDecl _ _ stmts _, state1) -> do
                let expr' = renameExpr vtable expr
                let cond' = renameExpr vtable' cond
                let mod'  = Moderator (Var t2 name' val2 pv2) op expr'
                let for1' = For2 inv (Var t name' (Just val') pv) mod' stmts cond' b pos
                return ( vtable
                        , ProcDecl namep args (for1':body) posp
                        , state1)

          Call name aargs pos -> -- (\a b -> getVar b vtable : a)
            return ( vtable
                  , ProcDecl namep args (Call name (foldr renameAArg [] aargs) pos : body) posp
                  , state)

          Uncall name aargs pos ->
            return ( vtable
                    , ProcDecl namep args
                        (Uncall name (foldr renameAArg [] aargs) pos : body)
                        posp
                    , state)

          Assert cond pos ->
            let cond' = renameExpr vtable cond in
              return ( vtable
                    , ProcDecl namep args (Assert cond' pos : body) posp
                    , state)

          Skip ->
            return ( vtable
                    , ProcDecl namep args (Skip : body) posp
                    , state)

          -- Not Globals can happen, as main should be the last declared procedure
          otherwise ->
            error $ "Renaming statement error: " ++ show otherwise ++ " should not happen here"

        renameAArg :: AArg -> [AArg] -> [AArg]
        renameAArg elm acc =
          case elm of
            VarAA (Var t name val pv) Nothing pos ->
              let tmp = tryGetVar name vtable in
                let name' = if (isJust tmp) then fromJust tmp else name in
                  VarAA (Var t name' val pv) Nothing pos : acc

            VarAA (Var t name val pv) (Just idxs) pos ->
              let tmp = tryGetVar name vtable in
                let name' = if (isJust tmp) then fromJust tmp else name in
                  let idxs' = renameExprs vtable idxs in
                    VarAA (Var t name' val pv) (Just idxs') pos : acc
            otherwise -> otherwise : acc

        insert :: Ident -> Int ->  Map Ident Ident -> Z3 (Map Ident Ident, Ident)
        insert n@(Ident nold pos) state vtable =
          let n' = Ident (show state ++ nold) pos in
            return (Map.insert n n' vtable, n')

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


processStatements :: [Stmt] -> Bool -> [ProcDecl] -> Vars -> Int -> [String] ->
  Z3 ([Stmt], Vars, Int, [String])
processStatements body doOpt ast scope state warnings =
  foldM (f ast) ([], scope, state, warnings) body
    >>= (\(body', scope', state', warnings') -> return (reverse body', scope', state', warnings'))
  where
    f :: [ProcDecl] -> ([Stmt], Vars, Int, [String]) -> Stmt -> Z3 ([Stmt], Vars, Int, [String])
    f ast (acc, scope, state, warnings) stmt = do
      (scope', stmt', warnings') <- processStatement doOpt ast scope stmt state warnings
      return (stmt':acc, scope', state + 1, warnings')


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
  -- (_, body', _, newWarnings) <- processStatements True ast initialScope body 0 warnings
  (body', _, _, newWarnings) <- processStatements (reverse body) True ast initialScope 0 warnings
  Z3.pop 1
  return (ProcDecl name args body' pos : accp, newWarnings)


processMainStore :: ProcDecl -> Z3 Vars
processMainStore (ProcDecl name _ body _) =
  case name of
    Ident "main" _ -> foldM f Map.empty $ reverse body
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
processProgram :: Program -> Z3 (Program, [String])
processProgram (Program decls) = do
  initialScope <- processMainStore $ head decls
  (decls', warnings) <- foldM (processProcedure (tail decls) initialScope) ([], []) decls
  return (Program $ reverse decls', reverse warnings)
