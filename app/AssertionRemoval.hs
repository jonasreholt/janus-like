{-# LANGUAGE LambdaCase #-}
module AssertionRemoval where

import Z3.Monad (AST, Z3, (+?))
import qualified Z3.Monad as Z3

import Control.Monad (foldM, (=<<), (<=<), (>>=))

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.List (find, nub, (\\))
import Data.Maybe

import Syntax
import AstReversing
import EvalExpr

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


-- If Nothing is returned the array being set should be invalidated
setArray :: Vars -> [Expr] -> Int -> AST -> Z3 (Either Ident AST)
setArray scope vals idx arr =
  case vals of
    [] -> return $ Right arr
    (hd:tl) -> do
      idx' <- Z3.mkFreshBvVar "arrIdx" intSz
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
            Just n' -> return $ Right n'
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
    _ -> error $ "Optimization expression: Expr not implemented yet " ++ show e


createITE :: AST -> Vars -> Vars -> Vars -> Z3 Vars
createITE cond orig scope1 scope2 = foldM f orig $ Map.keys orig
  where f scope var = do
          -- During this phase it should always be Just, as we work on the origin scope
          let var1   = tryGetVar var scope1
          let var2   = tryGetVar var scope2

          newVar    <- makeVar (IntegerT) var -- TODO: again a hack with the type
          ite       <- Z3.mkIte cond (fromJust var1) (fromJust var2)
          Z3.assert =<< Z3.mkEq newVar ite
          return $ Map.insert var newVar scope


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
processStatement :: Bool -> [ProcDecl] -> Vars -> Stmt -> Int -> [String] -> Z3 (Vars, Stmt, [String])
processStatement doOpt ast scope stmt state warnings =
  case stmt of
    Global (Var t name val _) _ -> do
      newVar <- makeVar (fromJust t) name

      processExpr scope (fromJust val) >>= \case
        Right newVal -> do
          Z3.assert =<< Z3.mkEq newVar newVal
          return (Map.insert name newVar scope, stmt, warnings)

        Left errvar ->
          return (scope, stmt, (show name ++ " used with invalid variable " ++ show errvar) : warnings)

    Global (Arr t name sz vals _) _ -> do
      newVar  <- makeArr (fromJust t) name
      setArray scope (fromJust vals) 0 newVar >>= \case
        Right newVar' ->
          return (Map.insert name newVar' scope, stmt, warnings)

        Left errvar ->
          return (scope, stmt, (show name ++ " used with invalid variable " ++ show errvar) : warnings)

    Local (Var t name val _) _ -> do
      newVar <- makeVar (fromJust t) name

      processExpr scope (fromJust val) >>= \case
        Right newVal -> do
          Z3.assert =<< Z3.mkEq newVar newVal
          return (Map.insert name newVar scope, stmt, warnings)

        Left errvar ->
          return (scope, stmt, (show name ++ " used with invalid variable " ++ show errvar) : warnings)

    DLocal (Var t name val _) _ ->
      if (doOpt) then
        -- val     <- processExpr scope (fromJust val)
        processExpr scope (fromJust val) >>= \case
          Right val ->
            case tryGetVar name scope of
              Just var -> do
                eq <- Z3.mkEq var val
                validated <- validate stmt eq
                case validated of
                  Left err    ->
                    -- Continue without the assertion having any effect on the analysis
                    return (scope, stmt, err:warnings)
                  Right True  -> return (scope, Skip, warnings)
                  Right False -> Z3.assert eq >> return (scope, stmt, warnings)

              Nothing ->
                -- The deallocated variable is invalidated so the information cannot be used further on
                return (scope, stmt, (show name ++ " has been invalidated") : warnings)

          Left errvar ->
            return (scope, stmt, (show name ++ " used with invalid variable " ++ show errvar) : warnings)
      
        
      else
        return (scope, stmt, warnings)

    Mod (Moderator (Var _ name _ _) op val) _ _ ->
      -- Moderation can only happen for integer types. This should be checked by a type checker
      mod stmt scope warnings

    Mod (Moderator (Arr _ name sz vals _) op val) idx _ ->
      case tryGetVar name scope of
        Just arr ->
          processExpr scope val >>= \case
            Right incrVal ->
              -- TODO: Only works for 1D arrays
              processExpr scope (head (fromJust idx)) >>= \case
                Right idx' -> do
                  oldVal  <- Z3.mkSelect arr idx'
                  newVal  <-
                    case op of
                      PlusEq -> Z3.mkBvadd oldVal incrVal
                      SubEq  -> Z3.mkBvsub oldVal incrVal
                      XorEq  -> Z3.mkBvxor oldVal incrVal
                  Z3.mkStore arr idx' newVal
                  return (scope, stmt, warnings)
                Left errvar ->
                  return (Map.delete name scope, stmt, (show name ++ " just got invalidated because of " ++ show errvar) : warnings)
            Left errvar ->
              return (Map.delete name scope, stmt, (show name ++ " just got invalidated because of " ++ show errvar) : warnings)
        Nothing ->
          return (scope, stmt, (show name ++ " has been invalidated") : warnings)

    Switch (Var t1 n1 _ _) (Var t2 n2 _ _) _ -> do
      -- newVar1 <- makeVar (fromJust t1) n1
      -- newVar2 <- makeVar (fromJust t2) n2
      newVar1 <- makeVar IntegerT n1 -- TODO: Stop this hardcoding of types
      newVar2 <- makeVar IntegerT n2 -- TODO: Stop this hardcoding of types

      case (tryGetVar n1 scope, tryGetVar n2 scope) of
        (Just n1', Just n2') -> do
          Z3.assert =<< Z3.mkEq newVar1 n2'
          Z3.assert =<< Z3.mkEq newVar2 n1'
          return (Map.insert n1 newVar1 $ Map.insert n2 newVar2 scope, stmt, warnings)

        _ ->
          -- One of the switched variables has been invalidated
          return (scope, stmt, ("Either " ++ show n1 ++ " or " ++ show n2 ++ " has been invalidated") : warnings)

    Switch (Arr t1 n1 _ _ _) (Arr t2 n2 _ _ _) _ -> do
      -- newVar1 <- makeVar (fromJust t1) n1
      -- newVar2 <- makeVar (fromJust t2) n2
      newVar1 <- makeVar IntegerT n1 -- TODO: Stop this hardcoding of types
      newVar2 <- makeVar IntegerT n2 -- TODO: Stop this hardcoding of types

      case (tryGetVar n1 scope, tryGetVar n2 scope) of
        (Just n1', Just n2') -> do
          Z3.assert =<< Z3.mkEq newVar1 n2'
          Z3.assert =<< Z3.mkEq newVar2 n1'
          return (Map.insert n1 newVar1 $ Map.insert n2 newVar2 scope, stmt, warnings)

        _ ->
          return (scope, stmt, ("Either " ++ show n1 ++ " or " ++ show n2 ++ " has been invalidated") : warnings)

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
                -- body1'  <- processStatements doOpt ast scope body1 0
                -- body2'  <- processStatements doOpt ast scope body2 0
                (body1', scope1, state1, warnings') <- processStatements body1 doOpt ast scope 0 warnings
                (body2', scope2, state2, warnings'') <- processStatements body2 doOpt ast scope 0 warnings'

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
            -- body1'  <- processStatements doOpt ast scope body1 0
            -- body2'  <- processStatements doOpt ast scope body2 0
            (body1', scope1, state1, warnings') <- processStatements body1 doOpt ast scope 0 warnings
            (body2', scope2, state2, warnings'') <- processStatements body2 doOpt ast scope 0 warnings'
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
              return (scope, Skip, warnings)
            Right False ->
              Z3.assert cond >> return (scope, stmt, warnings)

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
    mod (Mod (Moderator (Var _ name _ _) op val) _ _) scope warnings = do
      case tryGetVar name scope of
        Just oldVar -> do
          newVar      <- makeVar IntegerT name
          processExpr scope val >>= \case
            Right incrVal -> do
              newVal <-
                case op of
                  PlusEq -> Z3.mkBvadd oldVar incrVal
                  SubEq  -> Z3.mkBvsub oldVar incrVal
                  XorEq  -> Z3.mkBvxor oldVar incrVal
              Z3.assert =<< Z3.mkEq newVar newVal
              return (Map.insert name newVar scope, stmt, warnings)

            Left errvar ->
              return (Map.delete name scope, stmt, (show name ++ " just got invalidated because of " ++ show errvar) : warnings)

        _ ->
          return (scope, stmt, (show name ++ " used invalidated var") : warnings)
    
    mod _ _ _ = error "Please only use this function for processing modification!"

    -- If loop is not unrollable variables modified within loop body, and not asserted in
    -- loop invariant, will be invalidated, as we can no longer use any assertions on these.
    -- Returns (new scope, outer loop optimizable?, optimized body).
    processForLoop :: Bool -> Maybe Invariant -> Var -> [Stmt] -> Moderator -> Expr -> Pos -> [String] -> Z3 (Vars, Bool, [Stmt], [String])
    processForLoop forward inv var@(Var t name (Just val) p1) body m@(Moderator v op incr) cond pos warnings =
      case fromJust t of
        IntegerT ->
          case op of
            XorEq -> error "Xor for-loops not implemented yet"
            _ -> do
              -- creating initial loop variable
              z3var <- makeVar IntegerT name
              processExpr scope val >>= \case
                Right z3expr -> do
                  eq <- Z3.mkEq z3var z3expr
                  Z3.assert eq
                  let scope' = Map.insert name z3var scope

                  Z3.push
                  (body', scope'', state', warnings') <-
                    processStatements body doOpt ast scope' state warnings
                  Z3.pop 1

                  -- TODO: This is not safe to do yet!!
                  let start = evalConstantIntExpr val
                  let end   = evalConstantIntExpr cond
                  let incr'  = evalConstantIntExpr incr

                  -- determine loop optimizer method
                  let unrollable = varFreeExprs [val, cond, incr] && modificationFreeIncr name body
                  case (inv, unrollable) of
                    (Just (Invariant inv' _), True) -> do
                      (_, _, _, warnings'') <- invariant inv' body' z3var scope' warnings'

                      if (start > end) then
                        error "Loops utilizing overflow/underflow is not allowed yet"
                      else do
                        let bound = (end - start) `div` incr'
                        (scope'', validated, warnings''') <- unroll bound forward body' pos var z3expr m scope' state warnings''

                        if (validated) then
                          return (scope'', True, body', warnings''')
                        else
                          return (scope'', False, body', warnings''')

                    (Just (Invariant inv' _), False) ->
                      invariant inv' body' z3var scope' warnings'

                    (Nothing, True) -> do
                      -- optimize using unrolling only
                      if start > end then
                        error "Loops utilizing overflow/underflow is not allowed yet"
                      else do
                        let bound = (end - start) `div` incr'
                        (scope'', validated, warnings'') <- unroll bound forward body' pos var z3expr m scope' state warnings'

                        if (validated) then
                          return (scope'', True, body', warnings'')
                        else
                          return (scope'', False, body', warnings'')

                    _ ->
                      -- cannot optimize at all :(
                      invalidateVars (modifiedVars body) scope
                        >>= (\scope' -> return (scope', False, body', ("Loop could not be unrolled at " ++ show pos) : warnings))
                Left errvar ->
                  invalidateVars (modifiedVars body) scope
                    >>= (\scope' -> return (scope', False, body, (show name ++ " used invalid variable") : warnings))
        BooleanT -> error "Boolean for-loops not implemented yet"
      where
        varFreeExprs :: [Expr] -> Bool
        varFreeExprs = \case
          []            -> True
          (ConstE _:tl)      -> True && varFreeExprs tl
          (VarE _:tl)        -> False && varFreeExprs tl
          (Arith _ e1 e2:tl) -> varFreeExprs [e1, e2] && varFreeExprs tl
          (Not e:tl)         -> varFreeExprs [e] && varFreeExprs tl
          (Size n:tl)        -> False && varFreeExprs tl -- TODO: Should be True, if it worked properly
          (SkipE:tl)         -> True && varFreeExprs tl
        
        modificationFreeIncr :: Ident -> [Stmt] -> Bool
        modificationFreeIncr name1 = \case
          [] -> True
          (Mod (Moderator (Var _ name2 _ _) _ _) _ _:tl) ->
            if (name1 == name2) then False else modificationFreeIncr name1 tl
          (_:tl) -> modificationFreeIncr name1 tl 

        invariant :: Expr -> [Stmt] -> AST -> Vars -> [String] -> Z3 (Vars, Bool, [Stmt], [String])
        invariant inv body z3var loopScope warnings'  =
          {-
            Must prove invariant at:
              1. Initialization of loop.
              2. Maintenance - if true before an iteration also true after.
              3. Termination of loop! Otherwise the whole analysis does not matter.
          -}
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
                        This ALWAYS assumes that the decreasing/increasing variable is the loop variable
                      
                        Based on whether <id> starts higher or lower than its end value,
                        I can determine whether it's increasing or decreasing.
                        Now all that is needed is:
                          iteration start
                          (get value of <id>)
                          loop body
                          (assert <id> being either larger or less than start)
                      -}
                      case tryGetVar name scope of
                        Just z3var' -> do
                          z3ast <-
                            case op of -- TODO: using the operator might not be good enough e.g. i += -1
                              PlusEq -> Z3.mkBvugt z3var' z3var
                              _      -> Z3.mkBvult z3var' z3var
                          
                          validate stmt z3ast >>= \case
                            Right True ->
                              -- also true at termination. So create scope, invalidate changed variables not part of
                              -- invariant.
                              -- Optimizing the assertion away is not possible only with invariant.
                              Z3.pop 1
                              >> Z3.assert z3inv -- invariant is true so carry the information onwards.
                              >> invalidateVars (modifiedVars body \\ exprVars inv) scope
                              >>= (\scope' -> return (scope', False, body, warnings'))

                            _ ->
                              Z3.pop 1
                              >> invalidateVars (modifiedVars body) scope
                              >>= (\scope' -> return (scope', False, body,
                                    ("Loop invariant not true at termination " ++ show pos) : warnings'))
                        _ ->
                          Z3.pop 1 >>
                          return (scope, False, body,
                            ("Loop variable " ++ show name ++ " has been invalidated") : warnings')
                    _ ->
                      Z3.pop 1
                      >> invalidateVars (modifiedVars body) scope
                      >>= (\scope' -> return (scope', False, body,
                            ("Loop invariant not true at maintenance " ++ show pos) : warnings'))
                Right False ->
                  invalidateVars (modifiedVars body) scope
                  >>= (\scope' -> return (scope', False, body,
                        ("Loop invariant not true at initialization " ++ show pos) : warnings'))
                Left errmsg ->
                  invalidateVars (modifiedVars body) scope
                  >>= (\scope' -> return (scope', False, body, errmsg : warnings'))
            Left errvar ->
              invalidateVars (modifiedVars body) scope
                >>= (\scope' -> return (scope', False, body,
                        ("Loop invariant used invalidated variables at " ++ show pos) : warnings'))


    -- processForLoop :: Bool -> Var -> [Stmt] -> Moderator -> Expr -> Pos -> [String] -> Z3 (Vars, Bool, [Stmt], [String])
    -- processForLoop forward var@(Var t name (Just val) p1) body m@(Moderator v op incr) cond pos warnings =
    --   if (varFreeExprs [val, cond, incr] && modificationFreeIncr name body) then
    --     -- Safe to calculate unroll number and unroll
    --     case fromJust t of
    --       IntegerT -> do
    --         let start = evalConstantIntExpr val
    --         let end   = evalConstantIntExpr cond
    --         let incr'  = evalConstantIntExpr incr
    --         case op of
    --           XorEq  -> error "xor for loops not implemented yet"

    --           _ ->
    --             if start > end then error "Loops utilizing overflow/underflow is not allowed yet"
    --             else do
    --               let bound = (end - start) `div` incr'

    --               -- creating the initial variable
    --               z3var  <- makeVar (fromJust t) name
    --               processExpr scope val >>= \case
    --                 Right z3expr -> do
    --                   eq     <- Z3.mkEq z3var z3expr
    --                   Z3.assert eq

    --                   Z3.push
    --                   -- (scope', body', state') <- processStatements True ast (Map.insert name z3var scope) body state
    --                   (body', scope', state', warnings') <- processStatements body doOpt ast (Map.insert name z3var scope) state warnings
    --                   Z3.pop 1

    --                   (scope'', validated, warnings'') <- unroll bound forward body' pos var z3expr m scope' state' warnings'
    --                   if (validated) then
    --                     return (scope'', True, body', warnings'')
    --                   else
    --                     return (scope'', False, body', warnings'')
    --                 Left errvar ->
    --                   return (scope, False, body, (show name ++ " used invalid variable") : warnings)

    --       BooleanT -> error "Boolean for-loops not implemented yet"
    --   else --error "Loop not being unrollable not implemented yet"
    --   {-
    --     Must invalidate every every variable modified within the loop.
    --     This is done by removing them from scope.
    --   -}
    --     invalidateVars (modifiedVars body) scope
    --      >>= (\scope' -> return (scope', False, body, ("Loop could not be unrolled: " ++ show pos) : warnings))
    --   where
    --     varFreeExprs :: [Expr] -> Bool
    --     varFreeExprs = \case
    --       []            -> True
    --       (ConstE _:tl)      -> True && varFreeExprs tl
    --       (VarE _:tl)        -> False && varFreeExprs tl
    --       (Arith _ e1 e2:tl) -> varFreeExprs [e1, e2] && varFreeExprs tl
    --       (Not e:tl)         -> varFreeExprs [e] && varFreeExprs tl
    --       (Size n:tl)        -> False && varFreeExprs tl -- TODO: Should be True, if it worked properly
    --       (SkipE:tl)         -> True && varFreeExprs tl
        
    --     modificationFreeIncr :: Ident -> [Stmt] -> Bool
    --     modificationFreeIncr name1 = \case
    --       [] -> True
    --       (Mod (Moderator (Var _ name2 _ _) _ _) _ _:tl) ->
    --         if (name1 == name2) then False else modificationFreeIncr name1 tl
    --       (_:tl) -> modificationFreeIncr name1 tl 

        unroll :: Int -> Bool -> [Stmt] -> Pos -> Var -> AST -> Moderator -> Vars -> Int -> [String] -> Z3 (Vars, Bool, [String])
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
                Just z3var -> do
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
                  return (scope, False, ("Loop variable " ++ show name ++ " has been invalidated") : warnings)


    processFunctionCall :: Vars -> [AArg] -> ProcDecl -> Int -> [String] -> Z3 (Vars, Stmt, [String])
    processFunctionCall scope aargs p state warnings =  do
      p' <- renameProc Map.empty p state
      case p' of
        (vtable, ProcDecl _ fargs' body' _, _) -> do
          scope'  <- foldM connectArgument scope (zip aargs fargs')
          -- closure <- processStatements False ast scope' body' 0
          (_, scope'', _, warnings') <- processStatements body' False ast scope' 0 warnings
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
        renameArg :: (Map Ident Ident, ProcDecl, Int) -> FArg -> Z3 (Map Ident Ident, ProcDecl, Int)
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

          Size name ->
            let name' = tryGetVar name vtable in
              case name' of
                Just n  -> Size n
                Nothing -> Size name

          otherwise -> otherwise


        renameExprs :: Map Ident Ident -> [Expr] -> [Expr]
        renameExprs vtable = \case
          [] -> []
          (hd:tl) -> renameExpr vtable hd : renameExprs vtable tl


        renameStmt :: (Map Ident Ident, ProcDecl, Int) -> Stmt -> Z3 (Map Ident Ident, ProcDecl, Int)
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
                    , ProcDecl namep args (Switch (Var t name' val pv) (Var t2 name2' val2 pv2) pos : body) posp
                    , state)

          Switch (Arr t name sz val pv) (Arr t2 name2 sz2 val2 pv2) pos -> do
            let tmp = tryGetVar name vtable
            let name' = if (isJust tmp) then fromJust tmp else name
            let tmp2 = tryGetVar name2 vtable
            let name2' = if (isJust tmp2) then fromJust tmp2 else name2
            return ( vtable
                    , ProcDecl namep args (Switch (Arr t name' sz val pv) (Arr t2 name2' sz val2 pv2) pos : body) posp
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
                    return ( vtable -- Use old vtable as variables introduced in ite is in a tighter scope
                            , ProcDecl namep args (Ite condif' stmtsif condfi' stmtselse pos : body) posp
                            , state2)

          For1 inv (Var t name val pv) body (Moderator (Var t2 _ val2 pv2) op expr) cond b pos -> do
            (vtable', name') <- insert name state vtable
            let val'          = renameExpr vtable (fromJust val)
            tmp2             <- foldM renameStmt (vtable', ProcDecl namep [] [] posp, state + 1) body
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
            let val'          = renameExpr vtable (fromJust val)
            tmp2             <- foldM renameStmt (vtable', ProcDecl namep [] [] posp, state + 1) body
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
                    , ProcDecl namep args (Uncall name (foldr renameAArg [] aargs) pos : body) posp
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
          otherwise -> error $ "Renaming statement error: " ++ show otherwise ++ " should not happen here"
      

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
            VarAA (Var _ namea _ _) _ _ -> do
              case tryGetVar namea scope of
                Just var -> do
                  Z3.assert =<< Z3.mkEq newVar var
                  return $ Map.insert name newVar scope

                _ -> return scope

        ArrFA t name sz _ -> do
          newVar <- makeArr t name
          case aargs of
            VarAA (Var _ namea _ _) _ _ -> do
              case tryGetVar namea scope of
                Just var -> do
                  Z3.assert =<< Z3.mkEq newVar var
                  return $ Map.insert name newVar scope

                _ -> return scope

        ConstFA t name _ -> do
          newVar <- makeVar t name
          case aargs of
            ConstAA (IntegerV val) _ -> do
              z3Val <- Z3.mkBvNum intSz val
              Z3.assert =<< Z3.mkEq newVar z3Val
              return $ Map.insert name newVar scope

            _ -> error $ "Connecting Arguments: Actual argument " ++ show (aargs) ++ " does not match constant " ++ show fargs

    connectArgumentsBack :: Vars -> (AArg, FArg) -> Z3 Vars
    connectArgumentsBack scope (VarAA (Var _ name1 _ _) Nothing _, VarFA t name _) = do
      newVar <- makeVar t name1
      case tryGetVar name scope of
        Just oldVar -> do
          Z3.assert =<< Z3.mkEq newVar oldVar
          return $ Map.insert name1 newVar scope

        _ -> return scope

    connectArgumentsBack scope (VarAA (Arr _ name1 _ _ _) (Just idxs) _, ArrFA t name _ _) = do
      newVar <- makeArr t name1
      case tryGetVar name scope of
        Just oldVar -> do
          Z3.assert =<< Z3.mkEq newVar oldVar
          return $ Map.insert name1 newVar scope

        _ -> return scope

    connectArgumentsBack scope _ = return scope


processStatements :: [Stmt] -> Bool -> [ProcDecl] -> Vars -> Int -> [String] -> Z3 ([Stmt], Vars, Int, [String])
processStatements body doOpt ast scope state warnings =
  foldM (f ast) ([], scope, state, warnings) body
    >>= (\(body', scope', state', warnings') -> return (reverse body', scope', state', warnings'))
  where
    f :: [ProcDecl] -> ([Stmt], Vars, Int, [String]) -> Stmt -> Z3 ([Stmt], Vars, Int, [String])
    f ast (acc, scope, state, warnings) stmt = do
      (scope', stmt', warnings') <- processStatement doOpt ast scope stmt state warnings
      return (stmt':acc, scope', state + 1, warnings')
      -- return (scope', stmt' : acc, state + 1, warnings')


-- processStatement :: Bool -> [ProcDecl] -> Vars -> Stmt -> Int -> [String] -> Z3 (Vars, Stmt, [String])

-- processStatements :: Bool -> [ProcDecl] -> Vars -> [Stmt] -> Int -> Z3 (Vars, [Stmt], Int, [String])
-- processStatements doOpt ast scope body state = foldM (f ast) (scope, [], state) body
--   where
--     f :: [ProcDecl] -> (Vars, [Stmt], Int) -> Stmt -> Z3 (Vars, [Stmt], Int)
--     f ast (scope, acc, state) stmt = do
--       res <- processStatement doOpt ast scope stmt state
--       return (fst res, acc ++ [snd res], state + 1, [])


processArgs :: [FArg] -> Vars -> Z3 Vars
processArgs args scope = foldM f scope args
  where
    f :: Vars -> FArg -> Z3 Vars
    f vars arg =
      case arg of
        VarFA t n _ -> do
          newVar <- makeVar t n
          return $ Map.insert n newVar vars
        ArrFA t n _ _ -> do
          newArr <- makeArr t n
          return $ Map.insert n newArr vars
        ConstFA t n _ -> do
          newVar <- makeVar t n
          return $ Map.insert n newVar vars


processProcedure :: [ProcDecl] -> Vars -> ([ProcDecl], [String]) -> ProcDecl -> Z3 ([ProcDecl], [String])
processProcedure ast scope (accp, warnings) (ProcDecl name args body pos) = do
  -- Make sure assertions created for one procedure does not influence another
  Z3.push
  initialScope <- processArgs args scope
  -- (_, body', _, newWarnings) <- processStatements True ast initialScope body 0 warnings
  (body', _, _, newWarnings) <- processStatements body True ast initialScope 0 warnings
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
      Global (Var t name val _) _    -> do
        newVar <- makeVar (fromJust t) name
        return $ Map.insert name newVar scope
      Global (Arr t name sz val _) _ -> do
        newVar <- makeArr (fromJust t) name
        return $ Map.insert name newVar scope
      _ ->
        return scope


-- First decl in decls is the main function.
processProgram :: Program -> Z3 (Program, [String])
processProgram (Program decls) = do
  initialScope <- processMainStore $ head decls
  (decls', warnings) <- foldM (processProcedure (tail decls) initialScope) ([], []) decls
  return (Program $ reverse decls', reverse warnings)

-- show <$> evalZ3 (liftM snd (processStatements [] Map.empty (parseStatements "local int a = 2\na += 1\ndealloc int a = 3")))
