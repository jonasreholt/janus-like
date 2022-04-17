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
  IntegerT -> text "int"
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
  Great  -> char '<'
  GreatEq-> text "<="
  Less   -> char '>'
  LessEq -> text ">="
  NotEq  -> char '!'
  Eq     -> text "=="


formatOperatorM :: ModOp -> Doc
formatOperatorM = \case
  PlusEq -> text "+="
  SubEq  -> text "-="
  XorEq  -> text "^="


formatArrayIndices :: [Integer] -> Doc
formatArrayIndices sz = foldl (\a e -> brackets (integer e) <> a) empty sz

formatArrayIndicesE :: [Expr] -> Doc
formatArrayIndicesE sz = foldl (\a e -> brackets (formatExpr e) <> a) empty sz


formatExpr :: Expr -> Doc
formatExpr = \case
  ConstE (IntegerV v) -> integer v
  ConstE (BooleanV True) -> text "true"
  ConstE (BooleanV False) -> text "false"

  VarE (LVar (Ident n _)) -> text n
  VarE (Lookup (Ident n _) e) -> text n <> formatArrayIndicesE e

  Arith op e1 e2 ->
    formatExpr e1 <+> formatOperator op <+> formatExpr e2

  Not e -> char '!' <> parens (formatExpr e)

  -- TODO: Type hack right here man!!
  Size (Ident n _) ->
    parens $
    text "sizeof" <> parens (text n) <+> char '/' <+> text "sizeof" <> parens (formatType IntegerT)

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
    Global (Var t (Ident n _) e _) _ ->
      formatType (fromJust t) <+> text n <+>
      case e of
        Just e' ->
          equals <+> formatExpr e'
        Nothing -> empty
      <>
      semi

    Global (Arr t (Ident n _) s e _) _ ->
      formatType (fromJust t) <+> text n <+> formatArrayIndices (fromJust s) <+>
      case e of
        Just e' ->
          equals <+> braces (formatExprs (fromJust e))
        Nothing -> empty
      <>
      semi

    Local (Var t (Ident n _) e _) _ ->
      formatType (fromJust t) <+> text n <+>
      case e of
        Just e' ->
          equals <+> formatExpr e'
        Nothing -> empty
      <>
      semi

    Local (Arr t (Ident n _) s e _) _ ->
      formatType (fromJust t) <+> text n <+> formatArrayIndices (fromJust s) <+>
      case e of
        Just e' ->
          equals <+> braces (formatExprs e')
        Nothing -> empty
      <>
      semi

    DLocal (Var t (Ident n _) e _) _ ->
      assert (text n <+> equals <> equals <+> formatExpr (fromJust e)) <> semi
      <> semi

    DLocal (Arr t (Ident n _) s e _) _ ->
      -- Need to initialize an array before hand as arrays cannot be initialized inline
      -- TODO: Hack with naming
      formatType (fromJust t) <+> text "_tmp_" <> text n <+> equals <+>
      braces (formatExprs (fromJust e))
      $+$
      assert (text n <+> equals <> equals <+> text "_tmp_" <> text n)
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

    For1 inv var body mod cond b _ ->
      loop True var inv body mod cond b

    For2 inv var mod body cond b _ ->
      loop False var inv body mod cond b

    Call (Ident n _) args _ ->
      text n <> parens (formatAArgs args) <> semi

    Uncall (Ident n _) args _ ->
      text n <> parens (formatAArgs args) <> semi

    Assert e _ -> assert $ formatExpr e

    Skip -> empty
  where
    loop :: Bool -> Var -> Maybe Invariant -> [Stmt] -> Moderator -> Expr -> Bool -> Doc
    loop forward (Var t (Ident n _) e _) inv body (Moderator _ op e1) cond b =
      -- TODO: Decide whether invariants are solely for analysis or not
      (case inv of
         Just (Invariant e _) -> assert (formatExpr e)
         _ -> empty)
      $+$
      text "for"
      <+> formatType (fromJust t) <+> text n <+> equals <+> formatExpr (fromJust e)
      <>
      (if (not forward) then
        comma <+> text n <+> formatOperatorM op <+> formatExpr e1
      else
        empty)
      $+$
      lbrace
      $+$
      (case inv of
         Just (Invariant e _) -> spc <> assert (formatExpr e)
         _ -> empty)
      $+$
      formatStmts spc body
      $+$
      spc <>
      (if (b) then
         empty
      else
         assert (char '!' <> parens (text n <+> equals <> equals <+> formatExpr (fromJust e))))
      $+$
      (case inv of
         Just (Invariant e _) -> spc <> assert (formatExpr e)
         _ -> empty)
      $+$
      rbrace
      <+>
      (if (forward) then
         text n <+> formatOperatorM op <+> formatExpr e1 <> comma
        else empty)
      <+> text "until"
      <+>
      parens (text "dealloc" <+> formatType (fromJust t) <+> text n <+> equals <+> formatExpr cond)
      $+$
      (case inv of
         Just (Invariant e _) -> assert (formatExpr e)
         _ -> empty)


formatStmts :: Doc -> [Stmt] -> Doc
formatStmts spc stmts = foldl (formatStmt spc) empty stmts


formatFArg :: FArg -> Doc
formatFArg arg =
  case arg of
    VarFA t (Ident name _) _ ->
      formatType t <+> char '&' <> text name
    ArrFA t (Ident name _) sz _ ->
      formatType t <+> text name <> formatArrayIndices sz
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
  text "void" <+>
  text name <>
  parens (formatFArgs args)
  $+$
  lbrace
  $+$
  formatStmts (formatSpace 4) body
  $+$
  rbrace

formatProcedure' :: Doc -> (ProcDecl, ProcDecl) -> Doc
formatProcedure' acc (p, pr) =
  acc
  $+$
  formatProcedure p
  $+$ space $+$
  formatProcedure pr
  $+$ space


formatProcedureDefinition :: Doc -> (ProcDecl, ProcDecl) -> Doc
formatProcedureDefinition acc ((ProcDecl (Ident n _) a _ _), ProcDecl (Ident nr _) ar _ _) =
  acc
  $+$
  text "void" <+> text n <> parens (formatFArgs a) <> semi
  $+$
  text "void" <+> text nr <> parens (formatFArgs ar) <> semi


formatProgram :: Program -> Program -> Doc
formatProgram (Program ps) (Program psR) =
  case zip ps psR of
    (main:tl) ->
      foldl formatProcedureDefinition empty tl
      $+$ space $+$
      foldl formatProcedure' empty tl
      $+$
      formatProcedure (fst main)
    [] -> empty
