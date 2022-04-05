module Syntax where

import Text.Parsec.Pos
import Control.Monad
import Data.Maybe

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
  = Var (Maybe Type) Ident (Maybe Expr) Pos
  -- Arr type name size value pos
  | Arr (Maybe Type) Ident (Maybe Integer) (Maybe [Expr]) Pos
instance Show Var where show (Var _ name e _)            = show(name) ++ " " ++ show(e)
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
  | Not    Expr
  | Size   Ident
  | SkipE
  deriving (Show)

-- Actual parameters
data AArg
  = VarAA   Var (Maybe [Expr]) Pos -- Maybe Expr is index for array
  | ConstAA Val Pos
  deriving (Show)

-- Formal parameters
data FArg
  = VarFA   Type Ident Pos
  | ArrFA   Type Ident Integer Pos
  | ConstFA Type Ident Pos
  deriving (Show)

-- <Var> <ModOp>= <Expr>
data Moderator = Moderator Var ModOp Expr
  deriving (Show)

data Stmt
  -- int <(Var )>
  = Global Var Pos
  | Local  Var Pos
  | DLocal Var Pos
  | Mod    Moderator (Maybe [Expr]) Pos -- Expr is only used if Var is Arr type
  | Switch Var Var Pos
  | Ite    Expr [Stmt] Expr [Stmt] Pos
  | For1   Var [Stmt] Moderator Expr Pos
  | For2   Var Moderator [Stmt] Expr Pos
  | Call   Ident [AArg] Pos
  | Uncall Ident [AArg] Pos 
  | Assert Expr Pos
  -- | Mark   Stmt
  | Skip
  deriving (Show)

data ProcDecl = ProcDecl Ident [FArg] [Stmt] Pos
  deriving (Show)

data Program = Program [ProcDecl]
  deriving (Show)


getStmtVar :: Stmt -> Var
getStmtVar (Local var pos) = var
getStmtVar (DLocal var pos) = var



prettyPrintStmts :: String -> [Stmt] -> IO ()
prettyPrintStmts acc stmts = mapM_ (f acc) stmts
  where f :: String -> Stmt -> IO ()
        f acc stmt = --putStrLn $ acc ++ show stmt
          case stmt of
            Ite cond body1 ficond body2 pos -> do
              putStrLn $ acc ++ "if (" ++ show cond ++ ") {"
              prettyPrintStmts (acc ++ "    ") body1
              putStrLn $ acc ++ "} fi (" ++ show ficond ++ ")"
              putStrLn $ acc ++ "else {"
              prettyPrintStmts (acc ++ "    ") body2
              putStrLn $ acc ++ "} " ++ show pos 

            _ -> putStrLn $ acc ++ show stmt

argsToString :: [FArg] -> String
argsToString args = foldl f "" args
  where f :: String -> FArg -> String
        f acc arg = acc ++ ", " ++ show arg

prettyPrintProc :: ProcDecl -> IO ()
prettyPrintProc (ProcDecl name args body pos) =
  putStrLn (show name ++ "(" ++ argsToString args ++ ") {")
    >> prettyPrintStmts "    " body >> putStrLn "}"

prettyPrintPrgm :: Program -> IO ()
prettyPrintPrgm (Program decls) = mapM_ prettyPrintProc decls
-- mapM_ :: Monad m => (ProcDecl -> IO ()) -> [ProcDecl] -> IO ()
