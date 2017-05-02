{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module TripleTown where

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
   , reduceAt :: Int
   } deriving (Show, Eq)

type Height = Int
type Width = Int

printRow :: Board -> Int -> IO ()
printRow Board {..} row = do
  forM_ [1..width] $ \col ->
    case M.lookup (col,row) board of
      Nothing -> putChar '-'
      Just piece -> putChar (showPiece piece)
  putChar '\n'

showBoard :: Board -> IO ()
showBoard b@Board {..} =
  forM_ [1..height] (printRow b)

newBoard :: Int -> Int -> Board
newBoard height width =
  Board { board = mempty, reduceAt = 3, ..}

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

-- | Retrieve all positions for like-pieces in a path
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
          seen <- gets seen
          when (maybePiece == Just piece && pos' `notElem` seen) $ do
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
  zip positions $ map (flip M.lookup board) positions

getPosition :: StateT Board IO (Position, Piece)
getPosition = do
  b <- gets board
  h <- gets height
  w <- gets width
  piece <- getPiece
  pos <- liftIO $ fix $ \loop -> do
    putStrLn $ "Got: " ++ show piece
    putStrLn $ "Please enter a position between from\
     \ (1,1) to " ++ show (h,h) ++ " (i.e. (1,2)) to place it on the board"
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
            case M.lookup (x,y) b of
              Nothing -> pure (x,y)
              Just _ -> do
                putStrLn "Position already taken"
                loop
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

reduceBoard :: Monad m => Position -> Piece -> StateT Board m ()
reduceBoard position piece = do
  neighbors <- getNeighbors piece position
  wasReduced <- updateBoard position piece neighbors
  forM_ wasReduced $ \newPiece -> do
    newNeighbors <- getNeighbors newPiece position
    unless (null newNeighbors) (reduceBoard position newPiece)

runGame :: Board -> IO ()
runGame board = flip evalStateT board $ do
  fix $ \loop -> do
    (position, piece) <- getPosition
    insertIntoBoard position piece
    reduceBoard position piece
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
updateBoard
  :: Monad m
  => Position
  -> Piece
  -> [Position]
  -> StateT Board m (Maybe Piece)
updateBoard position piece positionsToRemove = do
  reductionCount <- gets reduceAt
  newBoard <- deleteAll positionsToRemove <$> get
  let newPiece = upgradePiece piece
  if length positionsToRemove >= reductionCount
    then do
      put newBoard {
        board = M.insert position newPiece (board newBoard)
      }
      pure (Just newPiece)
    else
      pure Nothing

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
