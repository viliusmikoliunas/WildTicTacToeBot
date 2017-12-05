module DataBoardSpace where
import BencodeWithoutListsParser(TicTacToeMessage(..))

import Data.Ix(range)
import Data.List(intersect,group,sort,(\\))
import Data.Maybe(catMaybes)
import Data.Char(toLower)

data BoardSpace = BoardSpace { x :: Int
                             , y :: Int
                             , s :: Char
                             } deriving (Show,Eq,Ord)

boardEdges :: [(Int,Int)]
boardEdges = [(1,0),(0,1),(2,1),(1,2)]

boardCorners :: [(Int,Int)]
boardCorners = [(0,0),(2,0),(0,2),(2,2)]

boardCenter :: (Int,Int)
boardCenter = (1,1)

boardSpaceGetX :: BoardSpace -> Int
boardSpaceGetX (BoardSpace x _ _) = x

boardSpaceGetY :: BoardSpace -> Int
boardSpaceGetY (BoardSpace _ y _) = y

boardSpaceGetCoordinatesOfSpace :: BoardSpace -> (Int,Int)
boardSpaceGetCoordinatesOfSpace (BoardSpace x y _) = (x,y)

boardSpaceGetSign :: BoardSpace -> Char
boardSpaceGetSign (BoardSpace _ _ sign) = sign

boardSpaceGetAllOccupiedEdges :: [BoardSpace] -> [BoardSpace]
boardSpaceGetAllOccupiedEdges occupiedSpaces = intersect (generateXOspaces boardEdges) occupiedSpaces

boardSpaceGetAllOccupiedCorners :: [BoardSpace] -> [BoardSpace]
boardSpaceGetAllOccupiedCorners occupiedSpaces = intersect (generateXOspaces boardCorners) occupiedSpaces

boardSpaceGetOccupiedCenter :: [BoardSpace] -> [BoardSpace]
boardSpaceGetOccupiedCenter occupiedSpaces = intersect (generateXOspaces [boardCenter]) occupiedSpaces

boardSpaceGetSignAtCoordinates :: [BoardSpace] -> (Int,Int) -> Maybe Char
boardSpaceGetSignAtCoordinates spaces (xc,yc) = do
  op <- boardSpaceGetByCoordinates spaces (xc,yc)
  return $ boardSpaceGetSign op

boardSpaceGetByCoordinates :: [BoardSpace] -> (Int,Int) -> Maybe BoardSpace
boardSpaceGetByCoordinates spaces (xc,yc) =
  if length answer == 1
    then Just $ head answer
    else Nothing
  where
    answer = filter ((== yc) . y) (filter ((== xc) . x) spaces)

generateXOspaces :: [(Int,Int)] -> [BoardSpace]
generateXOspaces boardSpaces = generateBoardSpaces boardSpaces ['x','o']

generateBoardSpaces :: [(Int,Int)] -> [Char] -> [BoardSpace]
generateBoardSpaces boardSpaces signList = do
  (x,y) <- boardSpaces
  z <- signList
  return (BoardSpace x y z)

boardSpaceAreSignsEqual :: [BoardSpace] -> Bool
boardSpaceAreSignsEqual spaces = all (== (head allSigns)) allSigns
  where
    allSigns = map boardSpaceGetSign spaces

boardSpaceCreateFromTicTacToeMessage :: [TicTacToeMessage] -> Either String [BoardSpace]
boardSpaceCreateFromTicTacToeMessage tttmessages = Right $ map makeBoardSpaceFromTicTacToeMessage tttmessages
  where
    makeBoardSpaceFromTicTacToeMessage :: TicTacToeMessage -> BoardSpace
    makeBoardSpaceFromTicTacToeMessage (Message (x,y) s _ ) = BoardSpace x y (toLower s)

boardSpaceGetRow :: [BoardSpace] -> Int -> [BoardSpace]
boardSpaceGetRow listOfBoardSpaces rowNumber = filter ((== rowNumber) . y) listOfBoardSpaces

boardSpaceGetColumn :: [BoardSpace] -> Int -> [BoardSpace]
boardSpaceGetColumn listOfBoardSpaces columnNumber = filter ((== columnNumber) . x) listOfBoardSpaces

boardSpaceGetDiagonal :: [BoardSpace] -> Int -> [BoardSpace]
boardSpaceGetDiagonal listOfBoardSpaces diagonalNumber =
  if diagonalNumber == 1
    then catMaybes $ map wrap [(0,0),(1,1),(2,2)]
    else if diagonalNumber == 2
      then catMaybes $ map wrap [(0,2),(1,1),(2,0)]
      else []
  where
    wrap :: (Int,Int) -> Maybe BoardSpace
    wrap coord = boardSpaceGetByCoordinates listOfBoardSpaces coord

boardSpaceOppositeEdge :: BoardSpace -> Maybe BoardSpace
boardSpaceOppositeEdge (BoardSpace 1 0 sign) = Just $ BoardSpace 1 2 sign
boardSpaceOppositeEdge (BoardSpace 1 2 sign) = Just $ BoardSpace 1 0 sign
boardSpaceOppositeEdge (BoardSpace 0 1 sign) = Just $ BoardSpace 2 1 sign
boardSpaceOppositeEdge (BoardSpace 2 1 sign) = Just $ BoardSpace 0 1 sign
boardSpaceOppositeEdge _ = Nothing

boardSpaceOppositeCorner :: BoardSpace -> Maybe BoardSpace
boardSpaceOppositeCorner (BoardSpace 0 0 sign) = Just $ BoardSpace 2 2 sign
boardSpaceOppositeCorner (BoardSpace 2 0 sign) = Just $ BoardSpace 0 2 sign
boardSpaceOppositeCorner (BoardSpace 2 2 sign) = Just $ BoardSpace 0 0 sign
boardSpaceOppositeCorner (BoardSpace 0 2 sign) = Just $ BoardSpace 2 0 sign
boardSpaceOppositeCorner _ = Nothing

boardSpaceGenerateAllPossibleMoves :: [BoardSpace] -> [BoardSpace]
boardSpaceGenerateAllPossibleMoves occupiedSpaces = generateBoardSpaces freeSpacesOnBoard ['x','o']
  where
    occupiedCoordinates = map boardSpaceGetCoordinatesOfSpace occupiedSpaces
    freeSpacesOnBoard = range((0,0),(2,2)) \\ occupiedCoordinates

listRemoveDuplicates :: (Ord a) => [a] -> [a]
listRemoveDuplicates = map head . group . sort
