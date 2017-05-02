module Main where

import           Control.Monad.State
import qualified Data.Map            as M
import           Test.Hspec
import           TripleTown

main :: IO ()
main = hspec $ do
  describe "TripleTown tests" $ do
    it "Should perform multiple reductions" $
      inputBoard `shouldBe` expectedBoard

inputBoard :: Board
inputBoard = flip execState inputBoard $ do
  insertIntoBoard (2,2) Grass
  reduceBoard (2,2) Grass
    where
      inputBoard :: Board
      inputBoard = Board 5 5 (M.fromList [
          ( (1,1), Bush )
        , ( (1,2), Bush )
        , ( (2,1), Grass )
        , ( (3,2), Grass )
        ]) 3

expectedBoard :: Board
expectedBoard =
  Board 5 5 (M.fromList [ ( (2,2), Tree ) ]) 3
