module Syntax2 where

import Data.Int (Int32)

-- data constructions symbolising AST of japa

data Pos = Pos Int Int
  deriving (Show)

data Type
  = IntegerT
  | BooleanT
  deriving (Show)

data Val
  = IntegerV Int32
  | BooleanV Bool
  deriving (Show)

data ModOp
  = PlusEq
  | SubEq
  | XorEq
  deriving (Show)

data BinOp
  -- Arithmetic operators
  = Plus | Sub | Xor | Mul | Div | Modulo | BAnd | BOr
  -- Boolean operators
  | And  | Or  | Great | GreatEq | Less | LessEq | NotEq | Eq
  deriving (Show)

data Ident = Ident String Pos
instance Show Ident where show (Ident name _) = name
instance Ord Ident  where Ident n1 _ <= Ident n2 _ = n1 <= n2
instance Eq Ident   where Ident n1 _ == Ident n2 _ = n1 == n2


data Var
  = Var Type Ident Pos
  -- Arr type name size pos
  | Arr Type Ident Int32 Pos
instance Show Var where show (Var _ name _)          = show(name)
                        show (Arr _ name _ _)        = show(name)
instance Ord Var  where Var _ n1 _ <= Var _ n2 _     = n1 <= n2
                        Arr _ n1 _ _ <= Arr _ n2 _ _ = n1 <= n2
instance Eq Var   where Var _ n1 _ == Var _ n2 _     = n1 == n2
                        Arr _ n1 _ _ == Arr _ n2 _ _ = n1 == n2

data LVar
  = LVar   Ident
  | Lookup Ident Expr
  deriving (Show)

data Expr
  = ConstE Val Pos
  | VarE   LVar Pos
  | Arith  Expr BinOp Expr Pos
  | Size   Ident Pos
  | SkipE
  deriving (Show)

-- Actual parameters
data AArg
  = VarAA   Var Pos
  | ConstAA Val Pos
  deriving (Show)

-- Formal parameters
data FArg
  = VarFA   Type Ident Pos
  | ArrFA   Type Ident Pos
  | ConstFA Type Ident Pos
  deriving (Show)

data Moderator = Moderator Var ModOp Expr
  deriving (Show)

data Stmt
  = Local  Var Expr Pos
  | DLocal Var Expr Pos
  | Mod    Moderator Expr Pos -- Expr is only used if Var is Arr type
  | Switch Var Var Pos
  | Ite    Expr [Stmt] Expr [Stmt] Pos
  | For1   Var Expr [Stmt] Moderator Expr Pos
  | For2   Var Expr Moderator [Stmt] Expr Pos
  | Call   Ident [AArg] Pos
  | Uncall Ident [AArg] Pos
  | Mark   Stmt
  | Skip
  deriving (Show)

data ProcDecl = ProcDecl Ident [FArg] [Stmt] Pos

data Program = Program [Var] [ProcDecl]


