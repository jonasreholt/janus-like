{-# LANGUAGE LambdaCase #-}
module Main where

import Criterion
import Criterion.Main (defaultMain)
import Z3.Monad (evalZ3)

import Compiler
import Syntax
import AssertionRemoval (processProgram)

forwardBackward :: (Program,Program) -> IO (Program, [String])
forwardBackward (ast1, ast2) = do
  evalZ3 $ processProgram ast1 True
  evalZ3 $ processProgram ast2 False


main :: IO ()
main = do
  -- prgm <- readFile "benchmarks/fib.japa"

  -- let ast = parseProgram prgm

  -- let astForward = typeCheckAnnotate ast

  -- let astBackward = reverseProgram astForward

  defaultMain
    [
      bgroup "fib.japa tests"
        [
          bench "Program" $ whnfAppIO compiler ["benchmarks/fib.japa"]
        , bench "Program (noOpt)" $ whnfAppIO compiler ["-noOpt", "benchmarks/fib.japa"]
        --   bench "Parsing" $ whnf parseProgram prgm
        -- , bench "Type Check" $ whnf typeCheckAnnotate ast
        -- , bench "Reverse Program" $ whnf reverseProgram astForward
        --  bench "Forward" $ whnfAppIO evalZ3 $ processProgram astForward True
        -- , bench "Backward" $ whnfAppIO evalZ3 $ processProgram astBackward False
        -- , bench "both" $ whnf forwardBackward (astForward, astBackward)
        -- , bench "Rename" $ whnf (rename astForward) "_forward"
        -- , bench "Formatting" $ whnf (formatProgram astForward) astBackward
        --  bench "Program" $ whnfAppIO bum ["benchmarks/fib.japa"]
        ]
      , bgroup "Factorial tests"
        [
          bench "Program" $ whnfAppIO compiler ["web/examples/factorial.japa"]
        , bench "Program (noOpt)" $ whnfAppIO compiler ["-noOpt", "web/examples/factorial.japa"]
        ]
      , bgroup "Perm-to-code tests"
        [
          bench "Program" $ whnfAppIO compiler ["web/examples/perm-to-code.japa"]
        , bench "Program (noOpt)" $ whnfAppIO compiler ["-noOpt", "web/examples/perm-to-code.japa"]
        ]
    ]
