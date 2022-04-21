{-# LANGUAGE LambdaCase #-}
module TypeCheckAnnotate where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.List (find)

import Debug.Trace

import Syntax
import EvalExpr

type Vtable = Map Ident Type
type Atable = Map Ident [Integer]
type Tables = (Vtable, Atable)



valueToType :: Val -> Type
valueToType = \case
  IntegerV _ -> IntegerT
  BooleanV _ -> BooleanT



reportTypeError :: Show b => Pos -> Type -> Type -> b -> a
reportTypeError p t1 t2 sinner =
  error $ "Type error at " ++ show p
  ++ ":\n  Expected " ++ show t1
  ++ "\n  Actual " ++ show t2
  ++ "\n    in expression: " ++ show sinner

reportTypeError' :: Type -> Type -> a
reportTypeError' t1 t2 =
  error $ "Type error:"
  ++ "\n  Expected " ++ show t1
  ++ "\n  Actual " ++ show t2


reportSizeError :: Pos -> Ident -> Integer -> Integer -> a
reportSizeError p n sz es =
  error $ "Mismatch on given size and expression at " ++ show p
  ++ "\n  Expected " ++ show sz
  ++ "\n  Actual " ++ show es


reportArrayIdxError :: Ident -> a
reportArrayIdxError n = error $ show n ++ " was looked up using non-integer"


reportNoTypeError :: Ident -> Pos -> a
reportNoTypeError n p = error $ show n ++ " not affiliated with a type at " ++ show p

reportNoSizeError :: Ident -> a
reportNoSizeError n = error $ show n ++ " not affiliated with size"




{-UndefinedT here means that the type is not defined by the operator-}
checkOp :: BinOp -> Type
checkOp = \case
  And     -> BooleanT
  Or      -> BooleanT
  Great   -> BooleanT
  GreatEq -> BooleanT
  Less    -> BooleanT
  LessEq  -> BooleanT
  NotEq   -> BooleanT
  Eq      -> BooleanT
  _       -> UndefinedT


checkExpr :: Tables -> Pos -> Expr -> (Expr, Type)
checkExpr tables@(vtable, atable) p expr = case expr of
  ConstE (IntegerV _) -> (expr, IntegerT)
  ConstE (BooleanV _) -> (expr, BooleanT)

  VarE (LVar n) -> case Map.lookup n vtable of
    Just t -> (expr, t)
    Nothing -> trace "1" $ reportNoTypeError n p

  VarE (Lookup n es) -> case Map.lookup n vtable of
    Just t -> case Map.lookup n atable of
      Just sz ->
        if sameSize sz es
        then let (es', tidx) = checkExprs tables p es in
               if tidx == IntegerT
               then (VarE (Lookup n es'), t)
               else reportArrayIdxError n
        else reportSizeError p n (arrSize sz) (toInteger (length es))
      Nothing -> trace "2" $ reportNoTypeError n p
    Nothing -> trace "3" reportNoTypeError n p

  Arith op e1 e2 ->
    let (e1', t1) = checkExpr tables p e1 in
      let (e2', t2) = checkExpr tables p e2 in
        if t1 == t2
        then case checkOp op of
          UndefinedT -> (Arith op e1' e2', t1)
          otherwise  -> (Arith op e1' e2', otherwise)
        else reportTypeError' t1 t2

  Not e -> (expr, BooleanT)

  Size n _ -> (Size n (Just IntegerT), IntegerT)

  SkipE -> (expr, UndefinedT)


checkExprs :: Tables -> Pos -> [Expr] -> ([Expr], Type)
checkExprs tables p exprs = foldr f ([], UndefinedT) exprs
  where
    f :: Expr -> ([Expr], Type) -> ([Expr], Type)
    f expr (acc, t) =
      let (expr', t') = checkExpr tables p expr in
        if t == UndefinedT || t == t'
        then (expr':acc, t')
        else reportTypeError p t t' expr




checkStmt :: [ProcDecl] -> (Tables, [Stmt]) -> Stmt -> (Tables, [Stmt])
checkStmt procs (tables@(vtable, atable), acc) stmt = case stmt of
  Global (Var (Just t) n (Just e) p1) p ->
    let (tables', e') = varDeclaration tables n t e p in
      (tables', (Global (Var (Just t) n (Just e') p1) p):acc)

  Global (Arr (Just t) n (Just s) (Just e) p1) p ->
    let (tables', e') = arrDeclaration tables n t s e p in
      (tables', (Global (Arr (Just t) n (Just s) (Just e') p1) p):acc)

  Local (Var (Just t) n (Just e) p1) p ->
    let (tables', e') = varDeclaration tables n t e p in
      (tables', (Local (Var (Just t) n (Just e') p1) p):acc)

  Local (Arr (Just t) n (Just s) (Just e) p1) p ->
    let (tables', e') = arrDeclaration tables n t s e p in
      (tables', (Local (Arr (Just t) n (Just s) (Just e') p1) p):acc)

  DLocal (Var (Just t) n (Just e) p1) p ->
    let told = fromJust (Map.lookup n vtable) in
      let ((vtable', atable'), e') = varDeclaration tables n t e p in
        let tnew = fromJust (Map.lookup n vtable') in
          if told == tnew
          then ((Map.delete n vtable, Map.delete n atable),
                (DLocal (Var (Just t) n (Just e') p1) p):acc)
          else reportTypeError p told tnew e'

  DLocal (Arr (Just t) n (Just s) (Just e) p1) p ->
    let told = fromJust (Map.lookup n vtable) in
      let ((vtable', atable'), e') = arrDeclaration tables n t s e p in
        let tnew = fromJust (Map.lookup n vtable') in
          if told == tnew
          then ((Map.delete n vtable, Map.delete n atable),
                (DLocal (Arr (Just t) n (Just s) (Just e') p1) p):acc)
          else reportTypeError p told tnew e'

  Mod (Moderator (Var _ n e1 p1) op e) Nothing p ->
    let (e', atype) = checkExpr tables p e in
      let etype = Map.lookup n vtable in
        case etype of
          Just etype' ->
            if atype == etype'
            then (tables,
                  (Mod (Moderator (Var (Just atype) n e1 p1) op e') Nothing p):acc)
            else reportTypeError p etype' atype e'
          Nothing -> trace "3" $ reportNoTypeError n p

  Mod (Moderator (Arr _ n _ e1 p1) op e) (Just idxs) p ->
    let (e', atype) = checkExpr tables p e in
      let etype = Map.lookup n vtable in
        case etype of
          Just etype' ->
            if atype == etype'
            then let (idxs', tidxs) = checkExprs tables p idxs in
                   let s = Map.lookup n atable in
                     if tidxs == IntegerT
                     then (tables,
                           (Mod (Moderator (Arr (Just atype) n s e1 p1) op e') (Just idxs') p):acc)
                     else reportArrayIdxError n
            else reportTypeError p etype' atype e'
          Nothing -> trace "4" $ reportNoTypeError n p

  Switch var1 var2 p -> case var1 of
    Var _ n1 e1 p1 -> case var2 of
      Var _ n2 e2 p2 -> case (Map.lookup n1 vtable, Map.lookup n2 vtable) of
        (Just t1, Just t2) ->
          if t1 == t2
          then (tables, (Switch (Var (Just t1) n1 e1 p1) (Var (Just t2) n2 e2 p2) p) : acc)
          else reportTypeError p t1 t2 stmt
        (Nothing, _) -> trace "5" $ reportNoTypeError n1 p
        (_, Nothing) -> trace "6" $ reportNoTypeError n2 p
      _ -> error $ "Cannot switch variables and arrays at " ++ show p

    Arr _ n1 _ e1 p1 -> case var2 of
      Arr _ n2 _ e2 p2 -> case (Map.lookup n1 vtable, Map.lookup n2 vtable) of
        (Just t1, Just t2) ->
          if t1 == t2
          then case (Map.lookup n1 atable, Map.lookup n2 atable) of
            (Just s1, Just s2) ->
              if arrEqual s1 s2
              then
                let v1 = Arr (Just t1) n1 (Just s1) e1 p1 in
                  let v2 = Arr (Just t2) n2 (Just s2) e2 p2 in
                    (tables, (Switch v1 v2 p) : acc)
              else error $ "Given arrays are not equal size at " ++ show p
            (Nothing, _) -> reportNoTypeError n1 p
            (_, Nothing) -> reportNoTypeError n2 p
          else reportTypeError p t1 t2 stmt
        _ -> error $ "Cannot switch variables and arrays at " ++ show p

  {-Japa is imperative so Ite does not have a return type.
    Hence the two branches have no return type-}
  Ite ifcond b1 ficond b2 p -> case (checkExpr tables p ifcond, checkExpr tables p ficond) of
    ((ifcond', BooleanT), (ficond', BooleanT)) ->
      let b1' = checkStmts procs tables b1 in
        let b2' = checkStmts procs tables b2 in
          (tables, (Ite ifcond' b1' ficond' b2' p) : acc)
    ((_, BooleanT), t2) -> reportTypeError p BooleanT (snd t2) ficond
    (t1, _) -> reportTypeError p BooleanT (snd t1) ifcond

  For1 inv var b m cond inf p ->
    let (var', inv', m', cond', b') = loop tables var inv b m cond p in
      (tables, (For1 inv' var' b' m' cond' inf p) : acc)

  For2 inv var m b cond inf p ->
    let (var', inv', m', cond', b') = loop tables var inv b m cond p in
      (tables, (For2 inv' var' m' b' cond' inf p) : acc)

  Call n args p ->
    let args' = call tables procs n args p in
      (tables, (Call n args' p) : acc)

  Uncall n args p ->
    let args' = call tables procs n args p in
      (tables, (Uncall n args' p) : acc)

  Assert _ _ ->
      (tables, stmt : acc)

  Skip -> (tables, stmt : acc)
  where
    varDeclaration :: Tables -> Ident -> Type -> Expr -> Pos -> (Tables, Expr)
    varDeclaration tables@(vtable, atable) n t e p =
      let (e', t2) = checkExpr tables p e in
        if t2 == t
        then ((Map.insert n t vtable, atable), e')
        else reportTypeError p t t2 e'

    arrDeclaration :: Tables -> Ident -> Type -> [Integer] -> [Expr] -> Pos -> (Tables, [Expr])
    arrDeclaration tables@(vtable, atable) n t s es p =
      let (es', t2) = checkExprs tables p es in
        if t2 == t
        then ((Map.insert n t vtable, Map.insert n s atable), es')
        else reportTypeError p t t2 es'

    {-returns annotated loop constructs-}
    loop :: Tables -> Var -> Maybe Invariant -> [Stmt] -> Moderator -> Expr -> Pos
      -> (Var, Maybe Invariant, Moderator, Expr, [Stmt])
    loop tables@(vtable, atable) var@(Var (Just t) n (Just e) p1) inv b (Moderator v op e2) cond p =
      -- Say BooleanT on nothing simply for conciseness
      let (inv'', tinv) = (case inv of
                    Just (Invariant inv' pi) ->
                      let (inv'', t) = checkExpr tables pi inv' in
                        (Just $ Invariant inv'' pi, t)
                    Nothing   -> (Nothing, BooleanT)) in
        case (checkExpr tables p cond, tinv) of
          ((cond', t1), BooleanT) ->
            let (tables', e') = varDeclaration tables n t e p1 in
              let b' = checkStmts procs tables' b in
                let (e2', tm) = checkExpr tables' p e2 in
                  let exptm = fromJust $ Map.lookup n (fst tables') in
                    if exptm == tm
                    then if exptm == t1
                         then
                           ( Var (Just t) n (Just e') p1
                           , inv''
                           , Moderator v op e2'
                           , cond'
                           , b')
                         else reportTypeError p1 exptm t1 var
                    else reportTypeError p1 exptm tm e2
          (_, t2) -> reportTypeError p BooleanT t2 inv

    {-returns annotated actual argument list-}
    call :: Tables -> [ProcDecl] -> Ident -> [AArg] -> Pos -> [AArg]
    call tables@(vtable, atable) procs n aargs p =
      case find (\(ProcDecl n1 _ _ _) -> n1 == n) procs of
        Just (ProcDecl _ fargs _ _) ->
          if equalLength fargs aargs
          then map (checkArg tables) (zip aargs fargs)
          else error $ "Size mismatch on call " ++ show p
        Nothing -> error $ "Called function not defined: " ++ show p
      where
        checkArg :: Tables -> (AArg, FArg) -> AArg
        checkArg (vtable, atable) (VarAA (Var _ n _ p1) i p2, ArrFA t _ sz _) =
          case Map.lookup n vtable of
            Just t' ->
              if t' == t
              then case Map.lookup n atable of
                Just sz' ->
                  let actualsz' = (case i of
                                     Just i' ->
                                       arrSize sz' `div` arrSize (take (length i') (reverse sz'))
                                     _ -> arrSize sz') in
                    if actualsz' == arrSize sz
                    then VarAA (Arr (Just t') n (Just sz') Nothing p1) i p2
                    else reportSizeError p1 n (arrSize sz) actualsz'
                Nothing -> reportNoSizeError n
              else reportTypeError' t t'
            Nothing -> reportNoTypeError n p1

        checkArg (vtable, atable) (VarAA (Var _ n e p1) i p, VarFA t _ _) =
          case Map.lookup n vtable of
            Just t' ->
              if t' == t
              then VarAA (Var (Just t) n e p1) i p
              else reportTypeError p t t' n
            Nothing -> reportNoTypeError n p

        checkArg (vtable, atable) (aarg@(ConstAA v _), ConstFA t _ _) =
          if valueToType v == t
          then aarg
          else reportTypeError' t (valueToType v)

        checkArg _ (aargs,fargs) =
          error $ "Mismatch on call function arguments in:"
          ++ "\n  Expected " ++ show fargs
          ++ "\n  Actual " ++ show aargs






checkStmts :: [ProcDecl] -> Tables -> [Stmt] -> [Stmt]
checkStmts procs tables body = snd $ foldl (checkStmt procs) (tables, []) body


mapArguments :: Tables -> [FArg] -> Tables
mapArguments tables@(vtable, atable) = \case
  [] -> tables
  ((VarFA t n _):tl) -> mapArguments (Map.insert n t vtable, atable) tl
  ((ArrFA t n sz _):tl) ->
    mapArguments (Map.insert n t vtable, Map.insert n sz atable) tl
  ((ConstFA t n _):tl) ->
    mapArguments (Map.insert n t vtable, atable) tl


createMainStore :: Tables -> Stmt -> Tables
createMainStore tables@(vtable, atable) = \case
  Global (Var (Just t) n e _) _ ->
    (Map.insert n t vtable, atable)

  Global (Arr (Just t) n (Just s) e _) _ ->
    (Map.insert n t vtable, Map.insert n s atable)

  otherwise -> tables

{-Only need to check body of procedure-}
checkProcDecl :: Tables -> [ProcDecl] -> ProcDecl -> ProcDecl
checkProcDecl tables procs (ProcDecl name args body p) =
  let initialTables = mapArguments tables args in
    ProcDecl name args ((checkStmts procs) initialTables body) p


checkProcDecls :: [ProcDecl] -> [ProcDecl]
checkProcDecls ps@(ProcDecl _ _ b _:_) =
  map (checkProcDecl (foldl createMainStore (Map.empty, Map.empty) b) ps) ps


typeCheckAnnotate :: Program -> Program
typeCheckAnnotate (Program ps) = Program $ checkProcDecls ps
