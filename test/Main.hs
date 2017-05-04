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
inputBoard = flip execState testBoard $ do
  insertIntoBoard (3,3) Grass
  reduceBoard (3,3) Grass
    where
      testBoard :: Board
      testBoard = Board 6 6 (M.fromList [
          ( (3,1), Grass )
        , ( (3,2), Grass )
        , ( (4,3), Bush )
        , ( (4,4), Bush )
        , ( (3,4), Tree )
        , ( (3,5), Tree )
        ]) 3

expectedBoard :: Board
expectedBoard =
  Board 6 6 (M.fromList [ ( (3,3), House ) ]) 3
