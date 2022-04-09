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
    Local var pos -> DLocal var pos : acc

    DLocal var pos -> Local var pos : acc

    Mod (Moderator var modOp expr) idxs pos ->
      Mod (Moderator var (reverseOp modOp) expr) idxs pos : acc

    Ite condif bodyif condfi bodyelse pos ->
      Ite condfi bodyif condif bodyelse pos : acc

    For1 inv var body mod cond b pos ->
      For2 inv var mod (foldl reverseStatement [] body) cond b pos : acc
    
    For2 inv var mod body cond b pos ->
      For1 inv var (foldl reverseStatement [] body) mod cond b pos : acc

    Call name args pos -> Uncall name args pos : acc

    Uncall name args pos -> Call name args pos : acc

    otherwise -> otherwise : acc
        

reverseProcedure :: ProcDecl -> [ProcDecl] -> [ProcDecl]
reverseProcedure (ProcDecl name args body pos) acc =
  let body' = foldl reverseStatement [] body in
    ProcDecl name args body' pos : acc


reverseProgram :: Program -> Program
reverseProgram (Program ps) =
  Program $ foldr reverseProcedure [] ps