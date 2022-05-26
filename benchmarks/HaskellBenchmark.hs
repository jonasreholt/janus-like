{-# LANGUAGE LambdaCase #-}
module Main where

import Criterion
import Criterion.Main (defaultMain)
import Z3.Monad (evalZ3)

import Compiler
import Syntax
import Parser (parseProgram)
import AssertionRemoval (processProgram)
import AstReversing (reverseProgram)
import RenameProcedures (rename)
import JapaToCpp
import TypeCheckAnnotate

forwardBackward :: Program -> Program -> IO (Program, [String])
forwardBackward ast1 ast2 = do
  evalZ3 $ processProgram ast1 True
  evalZ3 $ processProgram ast2 False


main :: IO ()
main = do
  prgm <- readFile "benchmarks/fib.japa"

  let ast = parseProgram prgm

  let astForward = typeCheckAnnotate ast

  let astBackward = reverseProgram astForward

  defaultMain
    [
      bgroup "fib.japa tests"
        [
         bench "Forward" $ whnfAppIO evalZ3 $ processProgram astForward True
        , bench "Backward" $ whnfAppIO evalZ3 $ processProgram astBackward False
        , bench "both" $ whnfAppIO (forwardBackward astForward) astBackward
        ]
      -- , bgroup "Factorial tests"
      --   [
      --     bench "Program" $ whnfAppIO compiler ["web/examples/factorial.japa"]
      --   , bench "Program (noOpt)" $ whnfAppIO compiler ["-noOpt", "web/examples/factorial.japa"]
      --   ]
      -- , bgroup "Perm-to-code tests"
      --   [
      --     bench "Program" $ whnfAppIO compiler ["web/examples/perm-to-code.japa"]
      --   , bench "Program (noOpt)" $ whnfAppIO compiler ["-noOpt", "web/examples/perm-to-code.japa"]
      --   ]
    ]
