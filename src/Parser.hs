{-# LANGUAGE LambdaCase #-}
module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token

import Data.Functor.Classes (liftEq2)
import Data.Functor.Identity

import Data.Maybe

import Syntax


japaDefinition :: Token.GenLanguageDef String st Identity
japaDefinition =
  Token.LanguageDef { Token.commentStart      = "/*",
                      Token.commentEnd        = "*/",
                      Token.nestedComments    = False,
                      Token.commentLine       = "//",
                      Token.identStart        = letter <|> char '_',
                      Token.identLetter       = alphaNum <|> char '_',
                      Token.opStart           = oneOf "*/+-^&|<=>!%",
                      Token.opLetter          = oneOf "",
                      Token.reservedNames     = [ "if", "fi", "else",
                                                  "for", "until",
                                                  "size",
                                                  "call", "uncall",
                                                  "procedure",
                                                  "const",
                                                  "int",
                                                  "bool", "true", "false",
                                                  "local",
                                                  "dealloc",
                                                  "assert",
                                                  "invariant"
                                                ],
                      Token.reservedOpNames   = [],
                      Token.caseSensitive     = True
                    }

-- Lexer definition
japaLexer :: Token.GenTokenParser String st Identity
japaLexer = Token.makeTokenParser japaDefinition

-- The lexer functionality
identifier :: Parser Ident
identifier = do
  pos <- getPosition
  ident <- Token.identifier japaLexer
  return $ Ident ident pos

reserved :: String -> Parser ()
reserved = Token.reserved japaLexer

operator :: Parser String
operator = Token.operator japaLexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp japaLexer

charLiteral :: Parser Char
charLiteral = Token.charLiteral japaLexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral japaLexer

natural :: Parser Integer
natural = Token.natural japaLexer

symbol :: String -> Parser String
symbol = Token.symbol japaLexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace japaLexer

parens :: Parser a -> Parser a      -- ()
parens = Token.parens japaLexer

braces :: Parser a -> Parser a      -- {}
braces = Token.braces japaLexer

brackets :: Parser a -> Parser a    -- []
brackets = Token.brackets japaLexer

comma :: Parser String
comma = Token.comma japaLexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Token.commaSep1 japaLexer

jType :: Parser Type
jType = (reserved "int" >> return IntegerT)
  <|> (reserved "bool" >> return BooleanT)
  <?> "Type"

moderator :: Parser ModOp
moderator = (reservedOp "+" >> return PlusEq)
  <|> (reservedOp "-" >> return SubEq)
  <|> (reservedOp "^" >> return XorEq)
  <?> "moderator"


fArg :: Parser FArg
fArg = variable
  <|> constant
  <?> "formal parameter"
  where
    variable :: Parser FArg
    variable = do
      pos  <- getPosition
      t    <- jType
      idnt <- identifier
      idx  <- optionMaybe $ many1 (brackets natural)
      case idx of
        Just exprs -> return $ ArrFA t idnt exprs pos
        Nothing    -> return $ VarFA t idnt pos
    constant :: Parser FArg
    constant = do
      pos <- getPosition
      reserved "const"
      t    <- jType
      idnt <- identifier
      return $ ConstFA t idnt pos

aArg :: Parser AArg
aArg = variable
  <|> array
  <|> constant
  <|> boolConstant
  <?> "actual argument"
  where
    variable :: Parser AArg
    variable = try $ do
      pos <- getPosition
      idnt <- identifier
      return $ VarAA (Var Nothing idnt Nothing pos) Nothing pos
    array :: Parser AArg
    array = do
      pos <- getPosition
      idnt <- identifier
      idx <- many1 $ brackets expression
      return $ VarAA (Arr Nothing idnt Nothing Nothing pos) (Just idx) pos
    constant :: Parser AArg
    constant = do
      pos <- getPosition
      val <- natural
      return $ ConstAA (IntegerV val) pos
    boolConstant :: Parser AArg
    boolConstant = do
      pos <- getPosition
      val <- ((reserved "true" >> return True) <|> (reserved "false" >> return False))
      return $ ConstAA (BooleanV val) pos

args :: (Parser a) -> Parser [a]
args p = do
  a <- many p
  case a of
    [] -> return []
    [hd] -> do
      as <- many $ (comma >> p)
      return $ hd : as


-- Expression parsing:
-- oprTable is in descending precedence
oprTable :: [[Operator String () Identity Expr]]
oprTable =  [[   Infix (reservedOp "*" >> return (Arith Mul)) AssocLeft,
                 Infix (reservedOp "/" >> return (Arith Div)) AssocLeft,
                 Infix (reservedOp "%" >> return (Arith Modulo)) AssocLeft
             ],
             [   Infix (reservedOp "+" >> return (Arith Plus)) AssocLeft,
                 Infix (reservedOp "-" >> return (Arith Sub)) AssocLeft
             ],
             [   Infix (try $ reservedOp "<" >> notFollowedBy (char '=') >> return (Arith Less)) AssocLeft,
                 Infix (reservedOp "<=" >> return (Arith LessEq)) AssocLeft,
                 Infix (try $ reservedOp ">" >> notFollowedBy (char '=') >> return (Arith Great)) AssocLeft,
                 Infix (reservedOp ">=" >> return (Arith GreatEq)) AssocLeft
             ],
             [   Infix (reservedOp "==" >> return (Arith Eq)) AssocLeft,
                 Infix (reservedOp "!=" >> return (Arith NotEq)) AssocLeft
             ],
             [   Infix (try $ reservedOp "&" >> notFollowedBy (char '&') >> return (Arith BAnd)) AssocLeft,
                 Infix (reservedOp "^" >> return (Arith Xor)) AssocLeft,
                 Infix (try $ reservedOp "|" >> notFollowedBy (char '|') >> return (Arith BOr)) AssocLeft
             ],
             [   Infix (reserved "&&" >> return (Arith And)) AssocLeft,
                 Infix (reserved "||" >> return (Arith Or)) AssocLeft,
                 Prefix (try $ reserved "!" >> notFollowedBy (char '=') >> return (Not))
             ]
            ]

expression :: Parser Expr
expression = buildExpressionParser oprTable term <?> "expression"

term :: Parser Expr
term =  parens expression
  <|> number
  <|> ((reserved "true" >> return (ConstE (BooleanV True)))
       <|> (reserved "false" >> return (ConstE (BooleanV False))))
  <|> lVal
  <|> size
  <?> "simple expression"
  where
    lVal :: Parser Expr
    lVal = do
      idnt <- identifier
      arrayLookup <- optionMaybe (many1 $ brackets $ expression)
      case arrayLookup of
        Just exprs -> return $ VarE (Lookup idnt exprs)
        Nothing   -> return $ VarE (LVar idnt)
    number :: Parser Expr
    number = do
      num <- natural
      return $ ConstE (IntegerV num)
    size :: Parser Expr
    size = do
      reserved "size"
      idnt <- parens (lVal)
      case idnt of
        VarE lvar -> return $ Size lvar IntegerT


-- Statement parsing
statement :: Parser Stmt
statement =
      local
  <|> dlocal
  <|> switch
  <|> mod
  <|> ite
  <|> loop
  <|> function "call"
  <|> function "uncall"
  <|> assert
  <?> "statement"
  where
    local :: Parser Stmt
    local = do
      pos <- getPosition
      reserved "local"
      pos1 <- getPosition
      t <- jType
      idnt <- identifier
      idxs <- optionMaybe $ many1 (brackets natural)
      reservedOp "="
      case idxs of
        Just idxs' -> do
          let sz = foldr (\a b -> a * b) 1 idxs'
          a <- arr' sz
          return $ Local (Arr (Just t) idnt (Just idxs') (Just a) pos1) pos
        Nothing -> do
          expr <- expression
          return $ Local (Var (Just t) idnt (Just expr) pos1) pos
    dlocal :: Parser Stmt
    dlocal = do
      pos <- getPosition
      reserved "dealloc"
      pos1 <- getPosition
      t <- jType
      idnt <- identifier
      idxs <- optionMaybe $ many1 (brackets natural)
      reservedOp "="
      case idxs of
        Just idxs' -> do
          let sz = foldr (\a b -> a * b) 1 idxs'
          a <- arr' sz
          return $ DLocal (Arr (Just t) idnt (Just idxs') (Just a) pos1) pos
        Nothing -> do
          expr <- expression
          return $ DLocal (Var (Just t) idnt (Just expr) pos1) pos
    switch :: Parser Stmt
    switch = try $ do -- Try on this as we need lookahead to distinguish with mod!
      pos <- getPosition
      idnt <- identifier
      reservedOp "<"; reservedOp "="; reservedOp ">"
      pos1 <- getPosition
      idnt2 <- identifier
      return $ Switch (Var Nothing idnt Nothing pos) (Var Nothing idnt2 Nothing pos1) pos
    mod :: Parser Stmt
    mod = do
      pos <- getPosition
      idnt <- identifier
      pos1 <- getPosition
      idxExpr <- optionMaybe (many1 $ brackets $ expression)
      opr <- moderator
      reservedOp "="
      expr <- expression
      case idxExpr of
        Just exprs ->
          return $ Mod (Moderator (Arr Nothing idnt Nothing Nothing pos1) opr expr) (Just exprs) pos
        Nothing    ->
          return $ Mod (Moderator (Var Nothing idnt Nothing pos1) opr expr) (Nothing) pos
    ite :: Parser Stmt
    ite = do
      pos <- getPosition
      reserved "if"
      cond1 <- parens expression
      body1 <- braces $ many $ statement -- statement block can be empty
      reserved "fi"
      cond2 <- parens expression
      ePart <- optionMaybe $ reserved "else"
      case ePart of
        Just () -> do -- ite
          body2 <- braces $ many $ statement
          return $ Ite cond1 body1 cond2 body2 pos
        Nothing -> -- it
          return $ Ite cond1 body1 cond2 [Skip] pos
    loop :: Parser Stmt
    loop = do
      pos <- getPosition
      reserved "for"
      loopvar <- local
      inv <- optionMaybe $ try invariant
      for2   <- optionMaybe comma
      case for2 of
        Just _ -> do
          posincr <- getPosition
          incr <- mod
          body <- braces $ many statement
          reserved "until"
          posuntil <- getPosition
          dvar <- parens dlocal
          return $ For2 inv (getStmtVar loopvar)
            (case incr of
               Mod modr idx pos -> modr
               _ -> error $ "for-loop incrementer is bad " ++ show posincr
            )
            body
            (case getStmtVar dvar of
               Var t n (Just e) p -> e
               _ -> error $ "for-loop until did not contain expression " ++ show posuntil
            )
            (LoopInfo False invariantStart)
            pos
        Nothing -> do
          body <- braces $ many statement
          posincr <- getPosition
          incr <- mod
          comma
          reserved "until"
          posuntil <- getPosition
          dvar <- parens dlocal
          return $ For1 inv (getStmtVar loopvar) body
            (case incr of
               Mod modr idx pos -> modr
               _ -> error $ "for-loop incrementer is bad " ++ show posincr
            )
            (case getStmtVar dvar of
               Var t n (Just e) p -> e
               _ -> error $ "for-loop until did not contain expression " ++ show posuntil
            )
            (LoopInfo False invariantStart)
            pos
      where
        invariant :: Parser Invariant
        invariant = do
          comma
          pos <- getPosition
          reserved "invariant"
          inv <- parens expression
          return $ Invariant inv pos


    function :: String -> Parser Stmt
    function kind = do
      pos <- getPosition
      reserved kind
      idnt <- identifier
      as <- parens $ args aArg
      case kind of
        "call" -> return $ Call idnt as pos
        "uncall" -> return $ Uncall idnt as pos
        _ -> error "Internal wrong call to \"function :: String -> Parser Stmt\""
    assert :: Parser Stmt
    assert = do
      pos <- getPosition
      reserved "assert"
      expr <- parens expression
      return $ Assert expr pos


-- Parsing procedure declarations
mainVariable :: Parser Stmt
mainVariable =
  mainArr
  <|> var
  <?> "global variable"
  where
    var :: Parser Stmt
    var = do
      pos  <- getPosition
      t    <- jType
      idnt <- identifier
      continues <- optionMaybe $ reservedOp "="
      case continues of
        Just () -> do
          expr <- expression
          return $ Global (Var (Just t) idnt (Just expr) pos) pos
        Nothing ->
          case t of
            IntegerT ->
              return $ Global (Var (Just t) idnt (Just (ConstE (IntegerV 0))) pos) pos
            BooleanT ->
              return $ Global (Var (Just t) idnt (Just (ConstE (BooleanV False))) pos) pos
    mainArr :: Parser Stmt
    mainArr = try $ do
      pos  <- getPosition
      t    <- jType
      idnt <- identifier
      sz   <- many1 $ brackets natural
      let arrsz = foldr (\a b -> fromIntegral a * b) 1 sz
      continues <- optionMaybe $ reserved "="
      case continues of
        Just _ -> do
          exprs <- arr' arrsz
          return $ Global
            (Arr (Just t) idnt (Just sz) (Just exprs) pos)
            pos
        Nothing ->
          return $ Global
            (Arr (Just t) idnt (Just sz) Nothing pos)
            pos


arr' :: Integer -> Parser [Expr]
arr' sz = do
  exprs <- braces g
  if (toInteger (length exprs) == sz) then
    return exprs
  else error "Main array declaration wrongfull"
  where
    numbers :: Parser [Expr]
    numbers = do
      hd <- expression
      tl <- many (comma >> expression)
      return $ hd:tl
    lists :: Parser [Expr]
    lists = do
      hd <- braces g
      tl <- many (comma >> braces g)
      return $ hd ++ (concat tl)
    g :: Parser [Expr]
    g = try lists <|> numbers <?> "Array initialization"


arr :: Parser Var
arr = try $ do
  pos  <- getPosition
  t    <- jType
  idnt <- identifier
  sz   <- many1 $ brackets natural
  continues <- optionMaybe $ reserved "="
  let arrsz = (foldr (\a b -> fromIntegral a * b) 1 sz)
  case continues of
    Just () -> do
      pos' <- getPosition
      exprs <- braces g
      if (toInteger (length exprs) == arrsz) then
        return $ Arr (Just t) idnt (Just sz) (Just exprs) pos
      else error $ "Main array declaration wrongfull " ++ show pos'
    Nothing ->
      return $ Arr (Just t) idnt (Just sz) Nothing
        pos
  where
    numbers :: Parser [Expr]
    numbers = do
      hd <- expression
      tl <- many (comma >> expression)
      return $ hd:tl
    lists :: Parser [Expr]
    lists = do
      hd <- braces g
      tl <- many (comma >> braces g)
      return $ hd ++ (concat tl)
    g :: Parser [Expr]
    g = try lists <|> numbers <?> "Array initialization"


procedure :: Parser ProcDecl
procedure = do
  pos <- getPosition
  reserved "procedure"
  idnt <- identifier
  as <- case idnt of
    Ident "main" _ -> parens f
    _ -> parens $ args fArg
  body <- case idnt of
    Ident "main"_ -> braces $ many (statement <|> mainVariable)
    _             -> braces $ many statement
  return $ ProcDecl idnt as body pos
  where
    f :: Parser [FArg]
    f = return []


-- Program consist of
--  procedures and one main procedures
--  The main procedure can have mainDeclarations
program :: Parser Program
program = do
  whiteSpace
  procs <- many1 procedure
  return $ Program procs


-- Parser
parseString :: Parser a -> String -> a
parseString parser input =
  case runParser parser () "" input of
    Left err -> error (show err)
    Right ast -> ast

parseExpression :: String -> Expr
parseExpression = parseString expression

parseStatement :: String -> Stmt
parseStatement = parseString statement

parseStatements :: String -> [Stmt]
parseStatements = parseString $ many1 statement

parseProcedure :: String -> ProcDecl
parseProcedure = parseString procedure

parseProgram :: String -> Program
parseProgram str =
  let ast = parseString program str in
    case ast of
      Program procs ->
        case foldl f (Nothing, []) procs of
          (Nothing, _) -> error "No main function in program"
          (Just p, ps) -> Program $ p : ps
  where f :: (Maybe ProcDecl, [ProcDecl]) -> ProcDecl -> (Maybe ProcDecl, [ProcDecl])
        f acc elm =
          case elm of
            ProcDecl (Ident "main" _) _ _ _ ->
              if (isJust $ fst acc) then
                error "Cannot define multiple main functions in one program"
              else
                (Just elm, snd acc)
            _ ->
                (fst acc, elm : snd acc)
