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



main :: IO ()
main = do
  defaultMain
    [
      bgroup "fib.japa tests"
        [
          bench "Program" $ whnfAppIO compiler ["benchmarks/fib.japa"]
        , bench "Program (noOpt)" $ whnfAppIO compiler ["-noOpt", "benchmarks/fib.japa"]
        ]
      , bgroup "Factorial tests"
        [
          bench "Program" $ whnfAppIO compiler ["benchmarks/factorial.japa"]
        , bench "Program (noOpt)" $ whnfAppIO compiler ["-noOpt", "benchmarks/factorial.japa"]
        ]
      , bgroup "Perm-to-code tests"
        [
          bench "Program" $ whnfAppIO compiler ["benchmarks/perm-to-code.japa"]
        , bench "Program (noOpt)" $ whnfAppIO compiler ["-noOpt", "benchmarks/perm-to-code.japa"]
        ]
      , bgroup "Encryption tests"
        [
          bench "Program" $ whnfAppIO compiler ["benchmarks/encryption.japa"]
        , bench "Program (noOpt)" $ whnfAppIO compiler ["-noOpt", "benchmarks/encryption.japa"]
        ]
      , bgroup "Run-length-encoder tests"
        [
          bench "Program" $ whnfAppIO compiler ["benchmarks/run-length-encoder.japa"]
        , bench "Program (noOpt)" $ whnfAppIO compiler ["-noOpt", "benchmarks/run-length-encoder.japa"]
        ]
    ]
