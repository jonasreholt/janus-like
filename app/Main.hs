module Main where

import System.Environment

import Compiler

main :: IO ()
main = do
    args <- getArgs
    compiler args