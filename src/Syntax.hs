{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module Syntax where

import Data.Map (Map)
import qualified Data.Map as Map

import Text.Parsec.Pos
import Control.Monad
import Data.Maybe
import Data.Bits

-- data constructions symbolising AST of japa
type Pos = SourcePos

data Type
  = IntegerT
  | BooleanT
  | UndefinedT
  deriving (Show, Eq)

data Val
  = IntegerV Integer
  | BooleanV Bool
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data Ident = Ident String Pos
instance Show Ident where show (Ident name _) = name
instance Ord Ident  where Ident n1 _ <= Ident n2 _ = n1 <= n2
instance Eq Ident   where Ident n1 _ == Ident n2 _ = n1 == n2


data Var
  = Var (Maybe Type) Ident (Maybe Expr) Pos
  -- Arr type name size value pos
  | Arr (Maybe Type) Ident (Maybe [Integer]) (Maybe [Expr]) Pos
  deriving (Show)
instance Ord Var  where Var _ n1 _ _ <= Var _ n2 _ _     = n1 <= n2
                        Arr _ n1 _ _ _ <= Arr _ n2 _ _ _ = n1 <= n2
instance Eq Var   where Var _ n1 _ _ == Var _ n2 _ _     = n1 == n2
                        Arr _ n1 _ _ _ == Arr _ n2 _ _ _ = n1 == n2

data LVar
  = LVar   Ident
  | Lookup Ident [Expr] -- [Expr] so one can lookup into nD arrays
  deriving (Show, Eq)

data Expr
  = ConstE Val
  | VarE   LVar
  | Arith  BinOp Expr Expr
  | Not    Expr
  | Size   LVar Type
  | SkipE
  deriving (Show, Eq)

-- Actual parameters
data AArg
  = VarAA   Var (Maybe [Expr]) Pos -- Maybe Expr is index for array
  | ConstAA Val Pos
  deriving (Show)

-- Formal parameters
data FArg
  = VarFA   Type Ident Pos
  | ArrFA   Type Ident [Integer] Pos
  | ConstFA Type Ident Pos
  deriving (Show)

-- <Var> <ModOp>= <Expr>
data Moderator = Moderator Var ModOp Expr
  deriving (Show)

data Invariant = Invariant Expr Pos
  deriving (Show)

data InvariantInfo =
    Initialization
  | Maintenance
  | Termination
  deriving (Show)

data LoopInfo = LoopInfo Bool Int
  deriving (Show)

getLoopInfoInv :: LoopInfo -> Int
getLoopInfoInv (LoopInfo _ i) = i

getLoopInfoBool :: LoopInfo -> Bool
getLoopInfoBool (LoopInfo b _) = b

data Stmt
  = Global Var Pos
  | Local  Var Pos
  | DLocal Var Pos
  | Mod    Moderator (Maybe [Expr]) Pos -- Expr is only used if Var is Arr type
  | Switch Var Var Pos
  | Ite    Expr [Stmt] Expr [Stmt] Pos
  | For1   (Maybe Invariant) Var [Stmt] Moderator Expr LoopInfo Pos
  | For2   (Maybe Invariant) Var Moderator [Stmt] Expr LoopInfo Pos
  | Call   Ident [AArg] Pos
  | Uncall Ident [AArg] Pos
  | Assert Expr Pos
  | Skip
  deriving (Show)

data ProcDecl = ProcDecl Ident [FArg] [Stmt] Pos
  deriving (Show)

data Program = Program [ProcDecl]
  deriving (Show)


-- | If this reports Nothing during processing it's because name is invalidated.
--   If it happens during renaming, it's because not all variables need to be renamed
tryGetVar :: Ident -> Map Ident a -> Maybe a
tryGetVar name scope =
  Map.lookup name scope


getProcDeclBody :: ProcDecl -> [Stmt]
getProcDeclBody (ProcDecl _ _ body _) = body


invariantStart :: Int
invariantStart = 7

invariantFlip :: Int -> InvariantInfo -> Int
invariantFlip prev = \case
  Initialization -> prev `xor` 1
  Maintenance    -> prev `xor` (shift 1 1)
  Termination    -> prev `xor` (shift 1 2)


isInvariantOn :: Int -> InvariantInfo -> Bool
isInvariantOn inv = \case
  Initialization -> (inv .&. 1) == 1
  Maintenance    -> (inv .&. (shift 1 1)) == 2
  Termination    -> (inv .&. (shift 1 2)) == 4


getStmtVar :: Stmt -> Var
getStmtVar (Local var pos) = var
getStmtVar (DLocal var pos) = var

getModVar :: Moderator -> Var
getModVar (Moderator var _ _) = var


getFArgName :: FArg -> Ident
getFArgName = \case
  VarFA _ n _ -> n
  ArrFA _ n _ _ -> n
  ConstFA _ n _ -> n

getFArgType :: FArg -> Type
getFArgType = \case
  VarFA t _ _ -> t
  ArrFA t _ _ _ -> t
  ConstFA t _ _ -> t

getVarType :: Var -> Maybe Type
getVarType = \case
  Var t _ _ _ -> t
  Arr t _ _ _ _ -> t

getVarName :: Var -> Ident
getVarName = \case
  Var _ n _ _ -> n
  Arr _ n _ _ _ -> n

getVarPos :: Var -> Pos
getVarPos = \case
  Var _ _ _ p -> p
  Arr _ _ _ _ p -> p


prettyPrintStmts :: String -> [Stmt] -> IO ()
prettyPrintStmts acc stmts = mapM_ (f acc) stmts
  where f :: String -> Stmt -> IO ()
        f acc stmt =
          case stmt of
            Ite cond body1 ficond body2 pos -> do
              putStrLn $ acc ++ "if (" ++ show cond ++ ") {"
              prettyPrintStmts (acc ++ "    ") body1
              putStrLn $ acc ++ "} fi (" ++ show ficond ++ ")"
              putStrLn $ acc ++ "else {"
              prettyPrintStmts (acc ++ "    ") body2
              putStrLn $ acc ++ "} " ++ show pos

            For1 inv var body mod cond b _ -> do
              putStrLn $ acc ++ "for1 (" ++ show b ++ ") " ++ "invariant (" ++ show inv ++ ") " ++ show var ++ " {"
              prettyPrintStmts (acc ++ "    ") body
              putStrLn $ acc ++ "} " ++ show mod ++ ", until (" ++ show cond ++ ")"

            For2 inv var mod body cond b _ -> do
              putStrLn $ acc ++ "for2 (" ++ show b ++ ") " ++ "invariant (" ++ show inv ++ ") " ++ show var ++ ", " ++ show mod ++ " {"
              prettyPrintStmts (acc ++ "    ") body
              putStrLn $ acc ++ "} until (" ++ show cond ++ ")"

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
