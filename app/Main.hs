module Main where

import System.Environment
import Z3.Monad (evalZ3)

import Parser (parseProgram)
import AssertionRemoval (processProgram)


usage :: String
usage = "Usage: Main [-noOpt] <program-name>"

checkArgs :: Bool -> [String] -> (Bool, IO String)
checkArgs doOpt args =
    case args of
        []      -> error usage
        [last]  -> (doOpt, readFile last)
        (hd:tl) -> if (hd == "-noOpt") then checkArgs False tl else error usage

-- The steps in compilation:
--      1. Parse src into AST
--      2. Reverse AST -> R(AST)
--      3. Optimize AST -> O(AST)
--      4. Optimize R(AST) -> O(R(AST))
--      5. Output both optimized ASTs into file
main :: IO ()
main = do
    args  <- getArgs
    let args' = checkArgs True args
    prgm  <- snd args'
    let ast = parseProgram prgm
    if (fst args') then do
        -- Optimize AST
        oAST <- evalZ3 $ processProgram ast
        putStrLn $ show oAST
    else
        -- Do not optimize AST
        putStrLn $ show ast
        







-- -- Below is garbage!!!!!!!!!!!!!!!
-- regularParse :: Parser a -> String -> Either ParseError a
-- regularParse p = parse p ""


-- tester :: Eq(a) => Parser a -> [(String, Either ParseError a)] -> Bool
-- tester parser sample =
--     case sample of
--         [] -> True
--         ((test, expected):tl) ->
--             if liftEq2 f g (regularParse parser test) expected then tester parser tl
--             else False
--     where
--         f :: ParseError -> ParseError -> Bool
--         f a b = a == b
--         g :: Eq(a) => a -> a -> Bool
--         g a b = a == b