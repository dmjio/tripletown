module Main where

import           TripleTown

main :: IO ()
main = do
  putStrLn "Welcome to Triple Town Console!"
  board <- createBoard
  showBoard board
  runGame board
