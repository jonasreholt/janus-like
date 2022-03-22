module Syntax where

import Text.Parsec.Pos

-- data constructions symbolising AST of japa
type Pos = SourcePos

data Type
  = IntegerT
  | BooleanT
  deriving (Show)

data Val
  = IntegerV Integer
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
  = Var Type Ident (Maybe Expr) Pos
  -- Arr type name size pos
  | Arr Type Ident Integer (Maybe Expr) Pos
instance Show Var where show (Var _ name _ _)            = show(name)
                        show (Arr _ name _ _ _)          = show(name)
instance Ord Var  where Var _ n1 _ _ <= Var _ n2 _ _     = n1 <= n2
                        Arr _ n1 _ _ _ <= Arr _ n2 _ _ _ = n1 <= n2
instance Eq Var   where Var _ n1 _ _ == Var _ n2 _ _     = n1 == n2
                        Arr _ n1 _ _ _ == Arr _ n2 _ _ _ = n1 == n2

data LVar
  = LVar   Ident
  | Lookup Ident [Expr] -- [Expr] so one can lookup into nD arrays
  deriving (Show)

data Expr
  = ConstE Val
  | VarE   LVar
  | Arith  BinOp Expr Expr
  | Size   Ident
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
  = Local  Var Pos
  | DLocal Var Pos
  | Mod    Moderator (Maybe Expr) Pos -- Expr is only used if Var is Arr type
  | Switch Var Var Pos
  | Ite    Expr [Stmt] Expr [Stmt] Pos
  | For1   Var [Stmt] Moderator Expr Pos
  | For2   Var Moderator [Stmt] Expr Pos
  | Call   Ident [AArg] Pos
  | Uncall Ident [AArg] Pos
  | Mark   Stmt
  | Skip
  deriving (Show)

data ProcDecl = ProcDecl Ident [FArg] [Stmt] Pos

data Program = Program [Var] [ProcDecl]


