{-# LANGUAGE LambdaCase #-}
module JapaToCpp where

import Prelude hiding ((<>))
import Text.PrettyPrint
import Data.Maybe

import Syntax


formatSpace :: Int -> Doc
formatSpace n
  | n <= 0 = empty
  | n >  0 = space <> formatSpace (n-1)


formatType :: Type -> Doc
formatType = \case
  IntegerT -> text "unsigned int"
  BooleanT -> text "bool"


formatOperator :: BinOp -> Doc
formatOperator = \case
  Plus -> char '+'
  Sub  -> char '-'
  Xor  -> char '^'
  Mul  -> char '*'
  Div  -> char '/'
  Modulo -> char '%'
  BAnd   -> char '&'
  BOr    -> char '|'
  And    -> text "&&"
  Or     -> text "||"
  Great  -> char '>'
  GreatEq-> text ">="
  Less   -> char '<'
  LessEq -> text "<="
  NotEq  -> text "!="
  Eq     -> text "=="


formatOperatorM :: ModOp -> Doc
formatOperatorM = \case
  PlusEq -> text "+="
  SubEq  -> text "-="
  XorEq  -> text "^="


formatArrayIndices :: [Integer] -> Doc
formatArrayIndices sz = foldr (\e a -> brackets (integer e) <> a) empty sz

formatArrayIndicesE :: [Expr] -> Doc
formatArrayIndicesE sz = foldr (\e a -> brackets (formatExpr e) <> a) empty sz


formatArrayValues :: [Expr] -> [Integer] -> Doc
formatArrayValues vals = \case
  [] -> empty
  [last] -> braces $ formatExprs vals
  (hd:tl) ->
    let vals' = splitEvery (length vals `div` fromIntegral hd) vals in
      braces $ f vals' (hd:tl)
  where
    f :: [[Expr]] -> [Integer] -> Doc
    f vals sz = case vals of
      [last] ->
        formatArrayValues last (tail sz)
      (hd:tl) ->
        formatArrayValues hd (tail sz) <> comma <> f tl sz

    -- Taken from https://hackage.haskell.org/package/list-grouping-0.1.1/docs/src/Data-List-Grouping.html#splitEvery
    -- | partitions list into sub-lists of length given by the Int:
    splitEvery :: Int -> [a] -> [[a]]
    splitEvery _ [] = []
    splitEvery n xs = as : splitEvery n bs
      where (as,bs) = splitAt n xs


formatExpr :: Expr -> Doc
formatExpr = \case
  ConstE (IntegerV v) -> integer v
  ConstE (BooleanV True) -> text "true"
  ConstE (BooleanV False) -> text "false"

  VarE (LVar (Ident n _)) -> text n
  VarE (Lookup (Ident n _) e) -> text n <> formatArrayIndicesE e

  Arith op e1 e2 ->
    parens (formatExpr e1)
    <+>
    formatOperator op
    <+>
    parens (formatExpr e2)

  Not e -> char '!' <> parens (formatExpr e)

  Size (Lookup (Ident n _) idx) t ->
    parens $
    text "sizeof" <> parens (text n <> formatArrayIndicesE idx) <+> char '/'
    <+> text "sizeof" <> parens (formatType t)

  Size (LVar (Ident n _)) t ->
    parens $
    text "sizeof" <> parens (text n) <+> char '/'
    <+> text "sizeof" <> parens (formatType t)

  SkipE -> empty

formatExpr' :: Doc -> Expr -> Doc
formatExpr' acc e = acc <> comma <+> formatExpr e

formatExprs :: [Expr] -> Doc
formatExprs [] = empty
formatExprs (hd:tl) = formatExpr hd <> foldl formatExpr' empty tl


assert :: Doc -> Doc
assert e = text "assert" <> parens e


formatAArg :: AArg -> Doc
formatAArg = \case
  VarAA (Var _ (Ident n _) _ _) _ _ ->
    text n
  VarAA (Arr _ (Ident n _) _ _ _) is _ ->
    case is of
      Just is' ->
        text n <+> formatArrayIndicesE is'
      Nothing ->
        text n
  ConstAA (IntegerV v) _ ->
    integer v
  ConstAA (BooleanV True) _ ->
    text "True"
  ConstAA (BooleanV False) _ ->
    text "False"

formatAArg' :: Doc -> AArg -> Doc
formatAArg' acc elm = acc <> comma <+> formatAArg elm

formatAArgs :: [AArg] -> Doc
formatAArgs [] = empty
formatAArgs args = formatAArg (head args) <> foldl formatAArg' empty (tail args)


formatStmt :: Doc -> Doc -> Stmt -> Doc
formatStmt spc acc stmt =
  acc $+$ spc <>
  case stmt of
    Local (Var t (Ident n _) e _) _ ->
      formatType (fromJust t) <+> text n <+>
      case e of
        Just e' ->
          equals <+> formatExpr e'
        Nothing -> empty
      <>
      semi

    Local (Arr t (Ident n _) s e _) _ ->
      formatType (fromJust t) <+> text n <> formatArrayIndices (fromJust s) <+>
      case e of
        Just e' ->
          equals <+> formatArrayValues e' (fromJust s)
        Nothing -> braces $ char '0'
      <>
      semi

    DLocal (Var t (Ident n _) e _) _ ->
      assert (text n <+> equals <> equals <+> formatExpr (fromJust e))
      <> semi

    DLocal (Arr t (Ident n _) (Just s) e _) _ ->
      -- Need to initialize an array before hand as arrays cannot be initialized inline
      -- TODO: Hack with naming
      text "// Asserting array equality" $+$
      formatType (fromJust t) <+> text "_tmp_" <> text n <> formatArrayIndices s <+> equals <+>
      formatArrayValues (fromJust e) s <> semi
      $+$
      assert (text "sizeof(_tmp_" <> text n <> rparen
             <+> equals<>equals <+>
             text "sizeof(" <> text n <> rparen
             <+> text "&&" <+>
             text "memcmp(" <> text "_tmp_" <> text n <> comma <+> text n <> comma <+>
             text "sizeof(" <> text n <> rparen <> rparen
             <+> equals<>equals <+> char '0'
             )
      <> semi

    Mod (Moderator (Var _ (Ident n _) _ _) op e) _ _ ->
      text n <+> formatOperatorM op <+> formatExpr e
      <> semi

    Mod (Moderator (Arr t (Ident n _) _ _ _) op e) is _ ->
      text n <> formatArrayIndicesE (fromJust is) <+> formatOperatorM op <+> formatExpr e
      <> semi

    Switch var1 var2 _ ->
      let n1 =
            case var1 of
              Var _ (Ident n _) _ _ -> n
              Arr _ (Ident n _) _ _ _ -> n
      in
        let n2 =
              case var2 of
                Var _ (Ident n _) _ _ -> n
                Arr _ (Ident n _) _ _ _ -> n
        in
          if (n1 == n2) then empty
          else
            text n1 <+> formatOperatorM XorEq <+> text n2
            <+> empty <+>
            text n2 <+> formatOperatorM XorEq <+> text n1
            <+> empty <+>
            text n1 <+> formatOperatorM XorEq <+> text n2

    Ite ifcond ifbody ficond elsebody _ ->
      text "if" <+> parens (formatExpr ifcond)
      $+$
      lbrace
      $+$
      formatStmts spc ifbody
      $+$
      (if ficond /= SkipE
      then spc <> assert (formatExpr ficond) <> semi
      else empty)
      $+$
      rbrace
      $+$
      case elsebody of
        [Skip] -> empty
        body ->
          text "else"
          $+$
          lbrace
          $+$
          formatStmts spc body
          $+$
          rbrace

    For1 inv var body mod cond inf _ ->
      loop False var inv body mod cond inf

    For2 inv var mod body cond inf _ ->
      loop True var inv body mod cond inf

    Call (Ident n _) args _ ->
      text n <> text "_forward" <> parens (formatAArgs args) <> semi

    Uncall (Ident n _) args _ ->
      text n <> text "_reverse" <> parens (formatAArgs args) <> semi

    Assert e _ -> assert (formatExpr e) <> semi

    _ -> empty
  where
    loop :: Bool -> Var -> Maybe Invariant -> [Stmt] -> Moderator -> Expr -> LoopInfo -> Doc
    loop forward (Var t (Ident n _) e _) inv body (Moderator _ op e1) cond inf =
      case isInvariantOn (getLoopInfoInv inf) Initialization of
        True  -> formatInvariant empty inv
        False -> empty
      $+$
      formatType (fromJust t) <+> text n <+> equals <+> formatExpr (fromJust e) <> semi
      $+$
      text "while" <> parens (text n <+> formatOperator (NotEq) <+> formatExpr cond)
      $+$
      lbrace
      $+$
      case isInvariantOn (getLoopInfoInv inf) Maintenance of
        True  -> formatInvariant spc inv
        False -> empty
      $+$
      case forward of
         True  -> spc <> text n <+> formatOperatorM op <+> formatExpr e1 <> semi
         False -> empty
      $+$
      formatStmts spc body
      $+$
      case forward of
        True  -> empty
        False -> spc <> text n <+> formatOperatorM op <+> formatExpr e1 <> semi
      $+$
      spc <>
      case getLoopInfoBool inf of
        True  -> empty
        False ->
          assert (char '!' <> parens (text n <+>equals<>equals<+> formatExpr (fromJust e))) <> semi
      $+$
      rbrace
      $+$
      case isInvariantOn (getLoopInfoInv inf) Termination of
        True  -> formatInvariant empty inv
        False -> empty

    formatInvariant :: Doc -> Maybe Invariant -> Doc
    formatInvariant spc = \case
      Just (Invariant inv _) -> spc <> assert (formatExpr inv) <> semi
      Nothing                -> empty


formatStmts :: Doc -> [Stmt] -> Doc
formatStmts spc stmts = foldl (formatStmt spc) empty stmts


formatFArg :: FArg -> Doc
formatFArg arg =
  case arg of
    VarFA t (Ident name _) _ ->
      formatType t <+> char '&' <> text name
    ArrFA t (Ident name _) sz _ ->
      -- make the array be passed by reference instead of value to avoid decaying into pointer
      -- to first element.
      formatType t <+> parens (char '&' <+> text name) <> formatArrayIndices sz
    ConstFA t (Ident name _) _ ->
      text "const" <+> formatType t <+> text name

formatFArg' :: Doc -> FArg -> Doc
formatFArg' acc arg =
  acc <> comma
  <+>
  formatFArg arg

formatFArgs :: [FArg] -> Doc
formatFArgs [] = empty
formatFArgs args = formatFArg (head args) <> foldl formatFArg' empty (tail args)


formatProcedure :: ProcDecl -> Doc
formatProcedure (ProcDecl (Ident name _) args body _) =
  case name of
    "main" -> text "int"
    _      -> text "void"
  <+>
  text name <>
  parens (formatFArgs args)
  $+$
  lbrace
  $+$
  formatStmts (formatSpace 4) body
  $+$
  rbrace

formatProcedure' :: (ProcDecl, ProcDecl) -> Doc -> Doc
formatProcedure' (p, pr) acc =
  acc
  $+$
  formatProcedure p
  $+$ space $+$
  formatProcedure pr
  $+$ space


formatProcedureDefinition :: (ProcDecl, ProcDecl) -> Doc -> Doc
formatProcedureDefinition ((ProcDecl (Ident n _) a _ _), ProcDecl (Ident nr _) ar _ _) acc =
  acc
  $+$
  text "void" <+> text n <> parens (formatFArgs a) <> semi
  $+$
  text "void" <+> text nr <> parens (formatFArgs ar) <> semi
  $+$ space

formatIncludes :: Doc
formatIncludes =
  text "#include <assert.h>"
  $+$ text "#include <cstring>"
  $+$ space $+$
  text "using namespace std;"


formatGlobalVar :: Doc -> Stmt -> Doc
formatGlobalVar acc stmt = acc $+$ case stmt of
  Global (Var t (Ident n _) e _) _ ->
    formatType (fromJust t) <+> text n <+>
    case e of
      Just e' ->
        equals <+> formatExpr e'
      Nothing -> empty
    <>
    semi

  Global (Arr t (Ident n _) s e _) _ ->
    formatType (fromJust t) <+> text n <> formatArrayIndices (fromJust s)
    <+> equals <+>
    case e of
      Just e' -> formatArrayValues e' (fromJust s)
      Nothing -> braces $ char '0'
    <>
    semi
  _ -> empty

formatGlobalVars :: [Stmt] -> Doc
formatGlobalVars body =
  if containGlobal body
  then text "// Global variables defining starting state"
       $+$ foldl formatGlobalVar empty body
  else empty
  where
    containGlobal :: [Stmt] -> Bool
    containGlobal = \case
      [] -> False
      (Global _ _:_) -> True
      (hd:tl) -> containGlobal tl



formatProgram :: Program -> Program -> Doc
formatProgram (Program ps) (Program psR) =
  case zip ps psR of
    (main:tl) ->
      formatIncludes
      $+$ space $+$
      formatGlobalVars (getProcDeclBody (fst main))
      $+$ space $+$
      foldr formatProcedureDefinition empty tl
      $+$ space $+$
      foldr formatProcedure' empty tl
      $+$
      formatProcedure (fst main)
    [] -> empty
