{-# LANGUAGE LambdaCase #-}
module AstReversing where

import Syntax


reverseOp :: ModOp -> ModOp
reverseOp = \case
  PlusEq -> SubEq
  SubEq  -> PlusEq
  XorEq  -> XorEq


reverseStatement :: [Stmt] -> Stmt -> [Stmt]
reverseStatement acc = \case
    Local  var pos -> DLocal var pos : acc
    DLocal var pos -> Local  var pos : acc

    Mod mod idxs pos ->
      Mod (reverseMod mod) idxs pos : acc

    Ite condif bodyif condfi bodyelse pos ->
      Ite
      condfi
      (foldl reverseStatement [] bodyif)
      condif
      (foldl reverseStatement [] bodyelse)
      pos
      : acc

    For1 inv (Var t n (Just e) p) body mod cond b pos ->
      For2 inv (Var t n (Just cond) p) (reverseMod mod)
      (foldl reverseStatement [] body)
      e b pos
      : acc
    For2 inv (Var t n (Just e) p) mod body cond b pos ->
      For1 inv (Var t n (Just cond) p)
      (foldl reverseStatement [] body)
      (reverseMod mod) e b pos
      : acc

    Call   name args pos -> Uncall name args pos : acc
    Uncall name args pos -> Call   name args pos : acc

    otherwise -> otherwise : acc
    where
      reverseMod :: Moderator -> Moderator
      reverseMod (Moderator var modOp expr) =
        Moderator var (reverseOp modOp) expr


reverseProcedure :: ProcDecl -> [ProcDecl] -> [ProcDecl]
reverseProcedure (ProcDecl name args body pos) acc =
  let body' = foldl reverseStatement [] body in
    ProcDecl name args body' pos : acc


reverseProgram :: Program -> Program
reverseProgram (Program ps) =
  let m = head ps in
    let rest = foldr reverseProcedure [] (tail ps) in
      Program $ m:rest
