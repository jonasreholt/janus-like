{-# LANGUAGE LambdaCase #-}
module RenameProcedure where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Syntax

type Vtable = Map Ident Ident
type State = Int

renameProc :: Vtable -> ProcDecl -> State -> (Vtable, ProcDecl, State)
renameProc vtable (ProcDecl name fargs body pos) state =
  let acc = foldr renameArg (vtable, ProcDecl name [] [] pos, state) fargs in
    let acc' = renameStmts acc body in
      acc'
  where
    renameArg :: FArg -> (Vtable, ProcDecl, State) -> (Vtable, ProcDecl, State)
    renameArg farg (vtable, ProcDecl namep args body posp, state) = case farg of
      VarFA t n@(Ident name p) pos ->
        let n' = Ident (show state ++ name) p in
          ( Map.insert n n' vtable
          , ProcDecl namep (VarFA t n' pos : args) body posp
          , state + 1 )

      ArrFA t n@(Ident name p) sz pos ->
        let n' = Ident (show state ++ name) p in
          ( Map.insert n n' vtable
          , ProcDecl namep (ArrFA t n' sz pos : args) body posp
          , state + 1)

      ConstFA t n@(Ident name p) pos ->
        let n' = Ident (show state ++ name) p in
          ( Map.insert n n' vtable
          , ProcDecl namep (ConstFA t n' pos : args) body posp
          , state + 1)

    renameExpr :: Vtable -> Expr -> Expr
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

    renameExprs :: Vtable -> [Expr] -> [Expr]
    renameExprs vtable = \case
      [] -> []
      (hd:tl) -> renameExpr vtable hd : renameExprs vtable tl

    renameStmt :: (Vtable, ProcDecl, State) -> Stmt -> (Vtable, ProcDecl, State)
    renameStmt acc@(vtable, p@(ProcDecl namep args body posp), state) stmt = case stmt of
      Local (Var t name val pv) pos ->
        let (vtable', name') = insert name state vtable in
          let val' = renameExpr vtable' (fromJust val) in
            ( vtable'
            , ProcDecl namep args (Local (Var t name' (Just val') pv) pos : body) posp
            , state + 1)

      Local (Arr t name sz vals pv) pos ->
        let (vtable', name') = insert name state vtable in
          let vals' = renameExprs vtable' (fromJust vals) in
            ( vtable'
            , ProcDecl namep args (Local (Arr t name' sz (Just vals') pv) pos : body) posp
            , state + 1)

      DLocal (Var t name val pv) pos ->
        let (vtable', name') = insert name state vtable in
          let val' = renameExpr vtable' (fromJust val) in
            ( vtable'
            , ProcDecl namep args (DLocal (Var t name' (Just val') pv) pos : body) posp
            , state + 1)

      DLocal (Arr t name sz vals pv) pos ->
        let (vtable', name') = insert name state vtable in
          let vals' = renameExprs vtable' (fromJust vals) in
            ( vtable'
            , ProcDecl namep args (DLocal (Arr t name' sz (Just vals') pv) pos : body) posp
            , state + 1)

      Mod (Moderator (Var t name val pv) op expr) _ pos ->
        let tmp = tryGetVar name vtable in
          let name' = if (isJust tmp) then fromJust tmp else name in
            let expr' = renameExpr vtable expr in
              let mod'  = Moderator (Var t name' val pv) op expr' in
                ( vtable
                , ProcDecl namep args (Mod mod' Nothing pos : body) posp
                , state)

      Mod (Moderator (Arr t name sz val pv) op expr) idxs pos ->
        let tmp = tryGetVar name vtable in
          let name' = if (isJust tmp) then fromJust tmp else name in
            let expr' = renameExpr vtable expr in
              let mod'  = Moderator (Arr t name' sz val pv) op expr' in
                let idxs' = renameExprs vtable (fromJust idxs) in
                  ( vtable
                  , ProcDecl namep args (Mod mod' (Just idxs') pos : body) posp
                  , state)

      Switch (Var t name val pv) (Var t2 name2 val2 pv2) pos ->
        let tmp = tryGetVar name vtable in
          let name' = if (isJust tmp) then fromJust tmp else name in
            let tmp2 = tryGetVar name2 vtable in
              let name2' = if (isJust tmp2) then fromJust tmp2 else name2 in
                ( vtable
                , ProcDecl namep args
                    (Switch (Var t name' val pv) (Var t2 name2' val2 pv2) pos : body)
                    posp
                , state)

      Switch (Arr t name sz val pv) (Arr t2 name2 sz2 val2 pv2) pos ->
        let tmp = tryGetVar name vtable in
          let name' = if (isJust tmp) then fromJust tmp else name in
            let tmp2 = tryGetVar name2 vtable in
              let name2' = if (isJust tmp2) then fromJust tmp2 else name2 in
                ( vtable
                , ProcDecl namep args
                    (Switch (Arr t name' sz val pv) (Arr t2 name2' sz val2 pv2) pos : body)
                    posp
                , state)

      Ite condif bodyif condfi bodyelse pos ->
        let condif' = renameExpr vtable condif in
          let condfi' = renameExpr vtable condfi in
            case renameStmts acc bodyif of
              (vtable1, ProcDecl _ _ stmtsif _, state1) ->
                case renameStmts acc bodyelse of
                  (vtable2, ProcDecl _ _ stmtselse _, state2) ->
                    -- Use old vtable as variables introduced in ite is in a tighter scope
                    ( vtable
                    , ProcDecl namep args
                        (Ite condif' stmtsif condfi' stmtselse pos : body)
                        posp
                    , state2)

      For1 inv (Var t name val pv) body' (Moderator (Var t2 _ val2 pv2) op expr) cond b pos ->
        let (vtable', name') = insert name state vtable in
          let val' = renameExpr vtable (fromJust val) in
            case renameStmts (vtable', ProcDecl namep [] [] posp, state + 1) body' of
              (vtablebody, ProcDecl _ _ stmts _, state1) ->
                let expr' = renameExpr vtable expr in
                  let cond' = renameExpr vtable' cond in
                    let mod' = Moderator (Var t2 name' val2 pv2) op expr' in
                      let for1' = For1 inv (Var t name' (Just val') pv) stmts mod' cond' b pos in
                        ( vtable
                        , ProcDecl namep args (for1':body) posp
                        , state1)

      For2 inv (Var t name val pv) (Moderator (Var t2 _ val2 pv2) op expr) body' cond b pos ->
        let (vtable', name') = insert name state vtable in
          let val' = renameExpr vtable (fromJust val) in
            case renameStmts (vtable', ProcDecl namep [] [] posp, state + 1) body' of
              (vtablebody, ProcDecl _ _ stmts _, state1) ->
                let expr' = renameExpr vtable expr in
                  let cond' = renameExpr vtable' cond in
                    let mod' = Moderator (Var t2 name' val2 pv2) op expr' in
                      let for1' = For2 inv (Var t name' (Just val') pv) mod' stmts cond' b pos in
                        ( vtable
                        , ProcDecl namep args (for1':body) posp
                        , state1)

      Call name aargs pos ->
        ( vtable
        , ProcDecl namep args (Call name (foldr renameAArg [] aargs) pos : body) posp
        , state)

      Uncall name aargs pos ->
        ( vtable
        , ProcDecl namep args
            (Uncall name (foldr renameAArg [] aargs) pos : body)
            posp
        , state)

      Assert cond pos ->
        let cond' = renameExpr vtable cond in
          (  vtable
          , ProcDecl namep args (Assert cond' pos : body) posp
          , state)

      Skip ->
        ( vtable
        , ProcDecl namep args (Skip : body) posp
        , state)

      -- Not Globals can happen, as main should be the last declared procedure
      otherwise ->
        error $ "Renaming statement error: " ++ show otherwise ++ " should not happen here"

    renameStmts :: (Vtable, ProcDecl, State) -> [Stmt] -> (Vtable, ProcDecl, State)
    renameStmts acc body =
      let (vtable, (ProcDecl n a b p), state) = foldl renameStmt acc body in
        (vtable, ProcDecl n a (reverse b) p, state)


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

    insert :: Ident -> Int ->  Map Ident Ident -> (Map Ident Ident, Ident)
    insert n@(Ident nold pos) state vtable =
      let n' = Ident (show state ++ nold) pos in
        (Map.insert n n' vtable, n')