module Syntax where

import Data.Int (Int32)

-- data constructions symbolising the abstract syntax of japa

data Pos
  = Pos Int Int
  deriving (Eq, Ord)

data Type
  = IntegerT Pos
  | BooleanT Pos

data Val
  = IntegerV Int32 Pos
  | BooleanV Bool Pos

data ModOp
  = PlusEq
  | SubEq
  | XorEq

data BinOp
  = Plus | Sub | Xor | Mul | Div | Modulo
  | BAnd | And | BOr | Or | Great | GreatEq
  | Less | LessEq | NotEq | Eq

data Ident
  = Ident String Pos

instance Ord Ident where
  Ident n1 _ < Ident n2 _ = n1 < n2
  Ident n1 _ <= Ident n2 _ = n1 <= n2
  Ident n1 _ > Ident n2 _ = n1 > n2
  Ident n1 _ >= Ident n2 _ = n1 >= n2
instance Eq Ident where
  (Ident n1 _) == (Ident n2 _) = n1 == n2

instance Show Ident where show (Ident s _) = s

data LVar
  = Var    Ident Pos
  | Lookup Ident Expr Pos

data Expr
  = ConstE  Val Pos
  | VarE    LVar Pos
  | Arith   Expr BinOp Expr Pos
  | Size    Ident Pos

data AArg
  = VarA  LVar Pos
  | Const Type Pos

data FArg
  = VarF Type Ident Pos
  | Arr  Type Ident Pos

data Moderator
  = Moderator Ident ModOp Expr Pos

data Stmt
  = Local  Type Ident Expr Pos -- VarDecl Pos
  | DLocal Type Ident Expr Pos
  | Mod    Moderator Pos
  | ModArr Ident Expr ModOp Expr Pos
  | Switch Ident Ident Pos
  | Ite    Expr [Stmt] Expr [Stmt] Pos
  | It     Expr [Stmt] Expr Pos
  | ForT   VarDecl [Stmt] Moderator Expr Pos
  | ForB   VarDecl Moderator [Stmt] Expr Pos
  | Call   Ident [AArg] Pos
  | Uncall Ident [AArg] Pos
  | Skip
  | Mark Stmt

data VarDef
  = VarDef Type Ident Pos
  | ArrDef Type Ident Expr Pos

data VarDecl
  = VarDecl LVar Pos

data ProcDecl
  = ProcDecl Ident [FArg] [Stmt]

data Program
  = Program [VarDef] [ProcDecl]
