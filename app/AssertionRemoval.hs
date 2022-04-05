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
import AstReversing

type Vars = Map Ident AST

intSz = 32

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

trd3 :: (a, b, c) -> c
trd3 (_, _, x) = x

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

-- Used during renaming, as not all variables need to be renamed
tryGetVar :: Ident -> Map Ident a -> Maybe a
tryGetVar name scope =
  Map.lookup name scope

getVar :: Ident -> Map Ident a -> String -> a
getVar n scope errorCode =
  case Map.lookup n scope of
    Just x -> x
    Nothing -> error $ "Variable " ++ show n ++ " could not be found! " ++ errorCode
      ++ foldl (\a b -> a ++ ", " ++ show b) "" (Map.keys scope)

-- getVar :: Ident -> Vars -> AST
-- getVar n scope =
--   case Map.lookup n scope of
--     Just x -> x
--     Nothing -> error $ "Variable " ++ show n ++ " could not be found!"


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
          return $ getVar n scope "While processing expression"
        _ -> error "VarE for array not implemented yet"
    Arith op e1 e2 -> do -- (+ (- 2 c) l)
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
          let var1  = getVar var scope1 "While calling createITE"
          let var2  = getVar var scope2 "While calling createITE"
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
processStatement :: Bool -> [ProcDecl] -> Vars -> Stmt -> Int -> Z3 (Vars, Stmt)
processStatement doOpt ast scope stmt state =
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
      if (doOpt) then do
        val     <- processExpr scope (fromJust val)
        let var = getVar name scope "While processing dealloc"
        eq <- Z3.mkEq var val
      
        Z3.push
        Z3.assert =<< Z3.mkNot eq
        validated <- validate
        if (validated) then Z3.pop 1 >> return (scope, Skip)
        else                Z3.pop 1 >> Z3.assert eq >> return (scope, stmt)
      else
        return (scope, stmt)

    Mod (Moderator (Var _ name _ _) op val) _ _ -> do
      -- Moderation can only happen for integer types. This should be checked by a type checker
      let oldVar  = getVar name scope "While processing moderator for var"
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
      let arr = getVar name scope "While processing moderator for array"
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
      let n1' =  getVar n1 scope "While processing switch for variables"
      let n2' =  getVar n2 scope "While processing switch for variables"
      Z3.assert =<< Z3.mkEq newVar1 n2'
      Z3.assert =<< Z3.mkEq newVar2 n1'
      return (Map.insert n1 newVar1 $ Map.insert n2 newVar2 scope, stmt)

    Switch (Arr t1 n1 _ _ _) (Arr t2 n2 _ _ _) _ -> do
      newVar1 <- makeVar (fromJust t1) n1 -- TODO: This requires a type check phase to annotate with types
      newVar2 <- makeVar (fromJust t2) n2
      let n1' =  getVar n1 scope "While processing switch for arrays"
      let n2' =  getVar n2 scope "While processing switch for arrays"
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
      ifcond' <- processExpr scope ifcond
      if (doOpt) then do
        -- if part
        Z3.push
        Z3.assert ifcond'
        body1'  <- processStatements doOpt ast scope body1
        ficond' <- processExpr (fst3 body1') ficond

        -- fi part
        Z3.assert =<< Z3.mkNot ficond'
        fiValidated <- validate
        Z3.pop 1

        -- else part
        Z3.push
        Z3.assert =<< Z3.mkNot ifcond'
        processStatements doOpt ast scope body2
        Z3.pop 1

        -- Need to create the scope of body1 and body2 again, because pop erased them
        -- TODO: Making change whole push pop above, to make sure we don't need this
        body1'  <- processStatements doOpt ast scope body1
        body2'  <- processStatements doOpt ast scope body2

        -- Continuation
        ite <- createITE ifcond' scope (fst3 body1') (fst3 body2')
        return (ite,
          Ite ifcond (snd3 body1') (if fiValidated then SkipE else ficond) (snd3 body2') pos)
      else do
        body1'  <- processStatements doOpt ast scope body1
        body2'  <- processStatements doOpt ast scope body2
        ite <- createITE ifcond' scope (fst3 body1') (fst3 body2')
        return (ite, stmt)

    Assert e _ -> do
      cond <- processExpr scope e

      Z3.push
      Z3.assert =<< Z3.mkNot cond
      validated <- validate
      Z3.pop 1

      Z3.assert cond >> return (scope, stmt)

    Call name aargs pos ->
      {-
          1. Rename formal parameters and local variables in called procedure.
          2. Connect FArg with AArg.
          3. Process the damn body,
          4. Connect parameters back again
      -}
      processFunctionCall scope aargs (findProc name ast) state

    Uncall name aargs pos ->
      let ast' = reverseProcedure (findProc name ast) [] in
        processFunctionCall scope aargs (head ast') state


    For1 var body mod cond pos -> error "Optimization statement: For1 not implemented yet"
    For2 var mod body cond pos -> error "Optimization statement: For2 not implemented yet"

    Skip -> return (scope, Skip)
    _ -> error $ "Optimization statement: Stmt not implemented yet - " ++ show stmt
  where
    processFunctionCall :: Vars -> [AArg] -> ProcDecl -> Int -> Z3 (Vars, Stmt)
    processFunctionCall scope aargs p state =  do
      p' <- renameProc Map.empty p state
      case p' of
        (vtable, ProcDecl _ fargs' body' _, _) -> do
          scope'  <- foldM connectArgument scope (zip aargs fargs')
          closure <- processStatements False ast scope' body'
          scope'' <- foldM connectArgumentsBack (fst3 closure) (zip aargs fargs')
          return $ (scope'', stmt)

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

          For1 (Var t name val pv) body (Moderator (Var t2 _ val2 pv2) op expr) cond pos -> do
            (vtable', name') <- insert name state vtable
            let val'          = renameExpr vtable (fromJust val)
            tmp2             <- foldM renameStmt (vtable', ProcDecl namep [] [] posp, state + 1) body
            case tmp2 of
              (vtablebody, ProcDecl _ _ stmts _, state1) -> do
                let expr' = renameExpr vtable expr
                let cond' = renameExpr vtable' cond
                let mod'  = Moderator (Var t2 name' val2 pv2) op expr'
                let for1' = For1 (Var t name' (Just val') pv) stmts mod' cond' pos
                return ( vtable
                        , ProcDecl namep args (for1':body) posp
                        , state1)

          For2 (Var t name val pv) (Moderator (Var t2 _ val2 pv2) op expr) body cond pos -> do
            (vtable', name') <- insert name state vtable
            let val'          = renameExpr vtable (fromJust val)
            tmp2             <- foldM renameStmt (vtable', ProcDecl namep [] [] posp, state + 1) body
            case tmp2 of
              (vtablebody, ProcDecl _ _ stmts _, state1) -> do
                let expr' = renameExpr vtable expr
                let cond' = renameExpr vtable' cond
                let mod'  = Moderator (Var t2 name' val2 pv2) op expr'
                let for1' = For2 (Var t name' (Just val') pv) mod' stmts cond' pos
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
              let var = getVar namea scope "While connecting actual arguments with formal - variables"
              Z3.assert =<< Z3.mkEq newVar var
              return $ Map.insert name newVar scope

        ArrFA t name sz _ -> do
          newVar <- makeArr t name
          case aargs of
            VarAA (Var _ namea _ _) _ _ -> do
              let var = getVar namea scope "While connecting actual arguments with formal - arrays"
              Z3.assert =<< Z3.mkEq newVar var
              return $ Map.insert name newVar scope

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
      let oldVar = getVar name scope "While connecting variable back"
      Z3.assert =<< Z3.mkEq newVar oldVar
      return $ Map.insert name1 newVar scope
      
      
      
      
      --  do
      -- let name'  = getVar name scope "While connecting var back"
      -- let name1' = getVar name1 scope "While connecting var back"
      -- Z3.assert =<< Z3.mkEq name' name1'

    connectArgumentsBack scope (VarAA (Arr _ name1 _ _ _) (Just idxs) _, ArrFA t name _ _) = do
      newVar <- makeArr t name1
      let oldVar = getVar name scope "While connecting array back"
      Z3.assert =<< Z3.mkEq newVar oldVar
      return $ Map.insert name1 newVar scope
      
      --  do
      -- let name'  = getVar name scope "While connecting array back"
      -- let name1' = getVar name1 scope "While connecting array back"
      -- Z3.assert =<< Z3.mkEq name' name1'

    connectArgumentsBack scope _ = return scope


processStatements :: Bool -> [ProcDecl] -> Vars -> [Stmt] -> Z3 (Vars, [Stmt], Int)
processStatements doOpt ast scope body = foldM (f ast) (scope, [], 0) body
  where
    f :: [ProcDecl] -> (Vars, [Stmt], Int) -> Stmt -> Z3 (Vars, [Stmt], Int)
    f ast (scope, acc, state) stmt = do
      res <- processStatement doOpt ast scope stmt state
      return (fst res, acc ++ [snd res], state + 1)


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


processProcedure :: [ProcDecl] -> Vars  -> ProcDecl -> Z3 ProcDecl
processProcedure ast scope (ProcDecl name args body pos) = do
  initialScope <- processArgs args scope
  body' <- processStatements True ast initialScope body
  return $ ProcDecl name args (snd3 body') pos


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
processProgram :: Program -> Z3 Program
processProgram (Program decls) = do
  initialScope <- processMainStore $ head decls
  decls' <- mapM (processProcedure (tail decls) initialScope) decls
  return $ Program decls'

-- show <$> evalZ3 (liftM snd (processStatements [] Map.empty (parseStatements "local int a = 2\na += 1\ndealloc int a = 3")))
