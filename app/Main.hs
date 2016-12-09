module Main where

import System.Environment
import DiamondKata

printUsage :: IO ()
printUsage = do
  putStrLn "Prints a Diamond Kata given a Character"
  putStrLn "Usage: ./<program name> <char>"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> case arg of
      [char]    -> if validChar char then putStr $ diamondKata char else printUsage
      otherwise -> printUsage
    otherwise -> printUsage
