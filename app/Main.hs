module Main where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token

import Data.Functor.Classes (liftEq2)
import Data.Functor.Identity

import Syntax


-- makeVar :: Ident -> Parser Ident
-- makeVar (Ident name p) = do
--     i <- getState
--     putState $ i + 1
--     pos <- getPosition
--     return $ Ident (show i ++ name) pos


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
                                                    "call", "uncall",
                                                    "procedure",
                                                    "const",
                                                    "int",
                                                    "bool", "true", "false",
                                                    "local",
                                                    "dealloc",
                                                    "assume"
                                                  ],
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

-- Expression parsing:
-- Descending precedence
oprTable :: [[Operator String () Identity Expr]]
oprTable =  [   [   Infix (reservedOp "*" >> return (Arith Mul)) AssocLeft,
                    Infix (reservedOp "/" >> return (Arith Div)) AssocLeft,
                    Infix (reservedOp "&" >> return (Arith Modulo)) AssocLeft
                ],
                [   Infix (reservedOp "+" >> return (Arith Plus)) AssocLeft,
                    Infix (reservedOp "-" >> return (Arith Sub)) AssocLeft
                ],
                [   Infix (reservedOp "<" >> return (Arith Less)) AssocLeft,
                    Infix (reservedOp "<=" >> return (Arith LessEq)) AssocLeft,
                    Infix (reservedOp ">" >> return (Arith Great)) AssocLeft,
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
                    Infix (reserved "||" >> return (Arith Or)) AssocLeft
                ]
            ]

expression :: Parser Expr
expression = buildExpressionParser oprTable term <?> "expression"

term :: Parser Expr
term = parens expression
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
            idnt <- parens identifier
            return $ Size idnt

-- Parser
parseString :: Parser a -> String -> a
parseString parser input =
    case runParser parser () "" input of
        Left err -> error (show err)
        Right ast -> ast

parseExpression :: String -> Expr
parseExpression = parseString expression


-- tmp :: Parser Var
-- tmp = (optionMaybe expression)

-- program :: Parser Program
-- program = do
--     whiteSpace  -- remove starting whitespace from program
--      -- parse global variables
--     -- parse procedures
--     eof         -- match eof after all procedures taken
--     -- prepare and return Program construct
--     where
--         globalDecl = do
--             pos <- getPosition
--             t <- jType
--             idnt <- identifier
--             -- either array or normal variable
--             liftM3 (Arr t idnt _ pos)
--                 <|> return $ Var t idnt pos




regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""


tester :: Eq(a) => Parser a -> [(String, Either ParseError a)] -> Bool
tester parser sample =
    case sample of
        [] -> True
        ((test, expected):tl) ->
            if liftEq2 f g (regularParse parser test) expected then tester parser tl
            else False
    where
        f :: ParseError -> ParseError -> Bool
        f a b = a == b
        g :: Eq(a) => a -> a -> Bool
        g a b = a == b

main :: IO ()
main = putStrLn "Hello, Haskell!"
