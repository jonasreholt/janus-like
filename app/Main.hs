module Main where

import System.Environment
import Z3.Monad (evalZ3)

import Syntax (prettyPrintPrgm)
import Parser (parseProgram)
import AssertionRemoval (processProgram)
import AstReversing (reverseProgram)
import RenameProcedures (rename)
import JapaToCpp


usage :: String
usage = "Usage: Main [-noOpt] <program-name>"

checkArgs :: Bool -> [String] -> (Bool, IO String)
checkArgs doOpt args =
    case args of
        []      -> error usage
        [last]  -> (doOpt, readFile last)
        (hd:tl) -> if (hd == "-noOpt") then checkArgs False tl else error usage

printWarnings :: [String] -> IO ()
printWarnings []      = return ()
printWarnings (hd:tl) = putStrLn ("Warning: " ++ hd) >> printWarnings tl

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
    let astForward = parseProgram prgm
    let astBackward = reverseProgram astForward
    if (fst args') then do
        -- Optimize AST
        (oASTF, warningsF) <- evalZ3 $ processProgram astForward
        (oASTR, warningsR) <- evalZ3 $ processProgram astBackward

        let oASTF' = rename oASTF "_forward"
        let oASTR' = rename oASTR "_reverse"

        printWarnings warningsF
        putStrLn ""
        putStrLn $ show $ formatProgram oASTF' oASTR'

    else do
        -- Do not optimize AST
        let astForward' = rename astForward "_forward"
        let astBackward' = rename astBackward "_reverse"

        putStrLn $ show $ formatProgram astForward' astBackward'






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
