{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment
import Z3.Monad (evalZ3)
import Data.Bits
import System.Timeout (timeout)
import System.Exit

import Syntax (prettyPrintPrgm)
import Parser (parseProgram)
import AssertionRemoval (processProgram)
import AstReversing (reverseProgram)
import RenameProcedures (rename)
import JapaToCpp


usage :: String
usage = "Usage: Main [-noOpt] <program-name>"


checkArgs :: (Int, Maybe String) -> [String] -> (Int, IO String)
checkArgs (options, file) = \case
  [] ->
    case file of
      Just file' -> (options, readFile file')
      Nothing ->
        case (options .&. (shift 1 1)) == 2 of
          True  -> (options, getContents)
          False -> error usage
  (hd:tl) ->
    case hd of
      "-noOpt" -> checkArgs (options .|. 1, file) tl
      "-stdin" -> checkArgs (options .|. (shift 1 1), file) tl
      _        -> checkArgs (options, Just hd) tl


printWarnings :: [String] -> IO ()
printWarnings []      = return ()
printWarnings (hd:tl) = putStrLn ("Warning: " ++ hd) >> printWarnings tl

timeOut :: IO a -> IO (Maybe a)
timeOut = timeout (60 * 1000000)

-- The steps in compilation:
--      1. Parse src into AST
--      2. Reverse AST -> R(AST)
--      3. Optimize AST -> O(AST)
--      4. Optimize R(AST) -> O(R(AST))
--      5. Output both optimized ASTs into file
main :: IO ()
main = do
    args  <- getArgs
    let args' = checkArgs (0, Nothing) args
    prgm  <- snd args'
    let astForward = parseProgram prgm
    let astBackward = reverseProgram astForward

    case ((fst args') .&. 1) == 0 of
      True -> do
        -- Optimize AST
        res1 <- timeOut $ evalZ3 $ processProgram astForward
        case res1 of
          Just (oASTF, warningsF) -> do
            res2 <- timeOut $ evalZ3 $ processProgram astBackward
            case res2 of
              Just (oASTR, warningsR) -> do
                let oASTF' = rename oASTF "_forward"
                let oASTR' = rename oASTR "_reverse"

                printWarnings warningsF
                putStrLn ""
                putStrLn $ show $ formatProgram oASTF' oASTR'

              Nothing ->
                exitWith $ ExitFailure 124
          Nothing ->
            exitWith $ ExitFailure 124

      False -> do
        let astForward' = rename astForward "_forward"
        let astBackward' = rename astBackward "_reverse"

        putStrLn $ show $ formatProgram astForward' astBackward'
