module Main where

import Control.Monad (when)
import TripleTown

main :: IO ()
main = do
  putStrLn "Welcome to Triple Town Console!"
  board <- createBoard
  showBoard board
  playAgain <- runGame board
  when playAgain main
