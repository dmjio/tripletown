{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Main where

import           Control.Monad
import           Control.Monad.State
import           Data.Char
import           Data.List
import qualified Data.Map            as M
import           Debug.Trace         (traceShow)
import           System.Random
import           Text.Read           (readMaybe)

data Piece
  = Grass
  | Bush
  | Tree
  | House
  deriving (Eq, Show, Enum)

showPiece :: Piece -> Char
showPiece Grass = 'G'
showPiece Bush  = 'B'
showPiece Tree  = 'T'
showPiece House = 'H'

getPiece :: MonadIO m => m Piece
getPiece = liftIO $ toEnum <$> randomRIO (0,2)

type Position = (Int, Int)

data Board = Board {
     height :: Int
   , width :: Int
   , board :: M.Map Position Piece
   } deriving Show

type Height = Int
type Width = Int

printRow :: Board -> Int -> IO ()
printRow Board {..} row = do
  forM_ [1..width] $ \c ->
    case M.lookup (row,c) board of
      Nothing -> putChar '-'
      Just piece -> putChar (showPiece piece)
  putChar '\n'

showBoard :: Board -> IO ()
showBoard b@Board {..} =
  forM_ [1..height] (printRow b)

newBoard :: Int -> Int -> Board
newBoard height width =
  Board { board = mempty, ..}

placePiece :: Piece -> Position -> Board -> Board
placePiece piece position existingBoard =
  existingBoard {
    board = M.insert position piece (board existingBoard)
  }

type Stack a = [a]

push :: a -> Stack a -> Stack a
push x xs = (x:xs)

pop :: Stack a -> (Maybe a, Stack a)
pop [] = (Nothing, [])
pop (x:xs) = (Just x, xs)

data NeighborState = NS {
   nodesToVisit :: Stack Position
 , neighbors :: [Position]
 , seen :: [Position]
 } deriving (Show)

addNeighbor :: Position -> State NeighborState ()
addNeighbor pos =
  modify $ \ns -> ns {
    neighbors = pos : neighbors ns
  }

addSeen :: Position -> State NeighborState ()
addSeen pos = modify $ \ns -> ns {
    seen = pos : seen ns
  }

emptyState :: NeighborState
emptyState = (NS [] [] [])

popStack :: State NeighborState (Maybe Position)
popStack = do
  ns <- get
  let (pos, newStack) = pop (nodesToVisit ns)
  put $ ns { nodesToVisit = newStack }
  return pos

pushStack :: Position -> State NeighborState ()
pushStack pos = do
  modify $ \ns -> ns {
    nodesToVisit = push pos (nodesToVisit ns)
  }

-- | Retrieve all pieces of a path
getNeighbors
  :: Monad m
  => Piece
  -> Position
  -> StateT Board m [Position]
getNeighbors piece pos = do
 b <- get
 pure $ neighbors $ execState (go b) emptyState {
   nodesToVisit = [pos]
 , neighbors = []
 } where
    go b = do
      pos <- popStack
      forM_ pos $ \foundPos -> do
        let neighbors = pieces b (getNeighborPositions foundPos)
        forM_ neighbors $ \(pos', maybePiece) -> do
          seen' <- gets seen
          when (maybePiece == Just piece && pos' `notElem` seen') $ do
            addSeen pos'
            pushStack pos'
            addNeighbor pos'
        go b

getNeighborPositions :: Position -> [Position]
getNeighborPositions (x,y) =
  [ (x + 1, y)
  , (x - 1, y)
  , (x, y + 1)
  , (x, y - 1)
  ]

pieces :: Board -> [Position] -> [(Position, Maybe Piece)]
pieces Board{..} positions =
  zip positions $
    map (flip M.lookup board) positions

getPosition :: StateT Board IO (Position, Piece)
getPosition = do
 h <- gets height
 w <- gets width
 piece <- getPiece
 pos <- liftIO $ fix $ \loop -> do
  putStrLn $ "Got: " ++ show piece
  putStrLn $ "Please enter a position between from\
   \ (1,1) to (" ++ show (h,h) ++ " (i.e. (1,2)) to place it"
  line <- getLine
  case readMaybe line :: Maybe (Int, Int) of
    Nothing -> do
      putStrLn "Parse error, please try again"
      loop
    Just (x,y) ->
      if x > w || y > h || x < 1 || y < 1
        then do
          putStrLn "Out of range, please try again"
          loop
        else
          pure (x,y)
 return (pos, piece)

insertIntoBoard
  :: MonadState Board m
  => Position
  -> Piece
  -> m ()
insertIntoBoard position piece = do
  modify $ \b -> b {
    board = M.insert position piece (board b)
  }

runGame :: Board -> IO ()
runGame board = flip evalStateT board $ do
  fix $ \loop -> do
    (position, piece) <- getPosition
    insertIntoBoard position piece
    let reduceBoard p = do
           neighbors <- getNeighbors p position
           newPiece <- updateBoard position p neighbors
           if null neighbors
             then return ()
             else reduceBoard newPiece
    reduceBoard piece
    board <- get
    liftIO $ showBoard board
    if isFull board
      then liftIO $ putStrLn "Game over!"
      else loop

upgradePiece :: Piece -> Piece
upgradePiece Grass = Bush
upgradePiece Bush = Tree
upgradePiece Tree = House
upgradePiece House = House

deleteAll :: [Position] -> Board -> Board
deleteAll [] b = b
deleteAll (x:xs) Board {..} =
  deleteAll xs $ Board { board = M.delete x board, .. }

-- | Reduces board
updateBoard :: Position -> Piece -> [Position] -> StateT Board IO Piece
updateBoard position piece positionsToRemove = do
  newBoard <- deleteAll positionsToRemove <$> get
  let newPiece = upgradePiece piece
  when (length positionsToRemove > 3) $
    put newBoard {
      board = M.insert position newPiece (board newBoard)
    }
  pure newPiece

isFull :: Board -> Bool
isFull Board {..} =
  height * width == M.size board

createBoard :: IO Board
createBoard = do
  putStrLn "Please enter height and width of board (i.e. (10,10))"
  line <- getLine
  case readMaybe line :: Maybe (Int, Int) of
    Nothing -> do
      putStrLn "Invalid parse try again"
      createBoard
    Just result ->
      case result of
        (x,y)
          | x < 0 || y < 0 ->
              putStrLn "Invalid board size" >> createBoard
          | otherwise -> pure (newBoard x y)

main :: IO ()
main = do
  putStrLn "Welcome to Triple Town Console!"
  board <- createBoard
  showBoard board
  runGame board

