{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module TripleTown where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.State
import           Data.Char
import           Data.List
import qualified Data.Map            as M
import           Data.Maybe
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
     height   :: Int
   , width    :: Int
   , board    :: M.Map Position Piece
   , reduceAt :: Int
   } deriving (Show, Eq)

type Height = Int
type Width = Int

showBoard :: Board -> IO ()
showBoard b@Board {..} =
  forM_ [1..height] $ \y -> do
    forM_ [1..width] $ \x -> do
      case M.lookup (x, y) board of
        Nothing -> putChar '-'
        Just piece -> putChar (showPiece piece)
    putChar '\n'

newBoard :: Maybe Int -> Maybe Int -> Board
newBoard height width =
  Board { board = mempty
        , reduceAt = 3
        , height = fromMaybe 6 height
        , width = fromMaybe 6 width
        }

defaultBoard :: Board
defaultBoard = newBoard Nothing Nothing

type Stack a = [a]

push :: a -> Stack a -> Stack a
push x xs = (x:xs)

pop :: Stack a -> (Maybe a, Stack a)
pop [] = (Nothing, [])
pop (x:xs) = (Just x, xs)

data NeighborState = NS {
   nodesToVisit :: Stack Position
 , neighbors    :: [Position]
 , seen         :: [Position]
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
emptyState = NS [] [] []

popStack :: State NeighborState (Maybe Position)
popStack = do
  ns <- get
  let (pos, newStack) = pop (nodesToVisit ns)
  put ns { nodesToVisit = newStack }
  return pos

pushStack :: Position -> State NeighborState ()
pushStack pos = do
  modify $ \ns -> ns {
    nodesToVisit = push pos (nodesToVisit ns)
  }

-- | Retrieve all positions for like-pieces in a path
getNeighbors
  :: MonadState Board m
  => Piece
  -> Position
  -> m [Position]
getNeighbors piece position = do
 let newState = emptyState {
   nodesToVisit = [position]
 , neighbors = []
 }
 board <- get
 pure $ neighbors $ execState (go board) newState
  where
    go board = fix $ \loop -> do
      position <- popStack
      forM_ position $ \currentPosition -> do
        let neighbors = withPieces board (getNeighborPositions currentPosition)
        forM_ neighbors $ \(neighborPosition, neighborPiece) -> do
          seen <- gets seen
          when (neighborPiece == piece && neighborPosition `notElem` seen) $ do
            addSeen neighborPosition
            pushStack neighborPosition
            addNeighbor neighborPosition
        loop

getNeighborPositions :: Position -> [Position]
getNeighborPositions (x,y) =
  [ (x + 1, y)
  , (x - 1, y)
  , (x, y + 1)
  , (x, y - 1)
  ]

withPieces :: Board -> [Position] -> [(Position, Piece)]
withPieces Board{..} positions =
  [ (x,y)
  | (x, Just y) <-
      zip positions .
      map (flip M.lookup board) $ positions
  ]

getPositionSim :: StateT Board IO (Position, Piece)
getPositionSim = do
  Board {..} <- get
  piece <- liftIO getPiece
  (x,y) <- liftIO $ fix $ \loop -> do
    (x,y) <- (,) <$> randomRIO (1,6) <*> randomRIO (1,6)
    case M.lookup (x,y) board of
      Nothing -> do
        threadDelay (1000 * 1000)
        putStrLn "======"
        pure ((x,y), piece)
      Just _  -> loop
  pure (x,y)

getPosition :: StateT Board IO (Position, Piece)
getPosition = do
  Board {..} <- get
  piece <- liftIO getPiece
  pos <- liftIO $ fix $ \loop -> do
    putStrLn $ "Got: " ++ show piece
    putStrLn $ "Please enter a position between from\
      \ (1,1) to " ++ show (height, height) ++ " (i.e. (1,2)) to place it on the board"
    line <- getLine
    case readMaybe line :: Maybe (Int, Int) of
      Nothing -> do
        putStrLn "Parse error, please try again"
        loop
      Just (x,y) ->
        if x > width || y > height || x < 1 || y < 1
          then do
            putStrLn "Out of range, please try again"
            loop
          else
            case M.lookup (x,y) board of
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

reduceBoard
  :: MonadState Board m
  => Position
  -> Piece
  -> m ()
reduceBoard position piece = do
  neighbors <- getNeighbors piece position
  wasReduced <- updateBoard position piece neighbors
  forM_ wasReduced $ \newPiece -> do
    newNeighbors <- getNeighbors newPiece position
    unless (null newNeighbors) (reduceBoard position newPiece)

runGame :: Board -> IO ()
runGame board = flip evalStateT board $ do
  fix $ \loop -> do
    (position, piece) <- getPositionSim
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

updateBoard
  :: MonadState Board m
  => Position
  -> Piece
  -> [Position]
  -> m (Maybe Piece)
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

createBoard :: Monad m => m Board
createBoard = pure defaultBoard

