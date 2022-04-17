module RenameProcedures where

import Syntax

rename :: Program -> String -> Program
rename (Program ps) postfix = Program $ (head ps) : foldr f [] (tail ps)
  where
    f :: ProcDecl -> [ProcDecl] -> [ProcDecl]
    f (ProcDecl (Ident name pos) args body posp) acc =
      ProcDecl (Ident (name ++ postfix) pos) args body posp : acc
