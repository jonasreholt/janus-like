data Type
  = Int Integer SourcePos

baseVal :: Type -> Expr
baseVal (Int _ pos) = Number 0 pos

data ident =
  Ident String SorucePos

data DeclVal
  = VarDecl Expr
  | ArrayDecl [Maybe Integer] [Expr]

data Lval
  = Var Ident
  | Lookup Ident [Expr]

data ModOp
  = AddEq
  | SubEq
  | XorEq
  deriving (Eq, Show)

data BinaryOp
  = Plus | Sub | Xor | Mul | Div | Mod
  | BAnd | And | BOr | Or | Great
  | GreatEq | Less | LessEq | Not | Eq
  deriving (Eq, Ord, Show)

data Stmt
  = Local  Type Ident Expr SourcePos
  | DLocal Type Ident Expr SourcePos
  | Mod    Lval ModOp Expr SourcePos
  | Switch Ident Ident SourcePos
  | Ite    Expr [Stmt] Expr [Stmt] SourcePos -- for if just [] at second [stmt]
  | For    DLocal [stmt] Mod Bool Expr SourcePos -- Bool indicate mod before body or after
  | Call   Ident Args SourcePos
  | Uncall Ident Args SourcePos
  deriving (Eq)

data Expr
  = Const   Integer SourcePos
  | Boolean Bool SourcePos
  | Var     Lval SourcePos
  | Op      Expr BinaryOp Expr SourcePos
  | Size    Ident SourcePos
  deriving(Eq)
  

optimize :: japa AST -> bool
optimize t
  | 
