module Main where

import Lib

main :: IO ()
main = welcome


welcome :: IO ()
welcome = putStrLn "Please read README.md file for instructions on how to use."
