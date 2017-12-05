module PlayAlgorithm(getNextMoveMessage
                    ,checkIfGameIsOver
                    ,chooseNextMove) where
import BencodeWithoutListsParser
import DataBoardSpace

import Data.List ((\\))
import Data.Maybe(catMaybes,fromJust,isJust)

getNextMoveMessage :: String -> String -> Either String String
getNextMoveMessage message playerName = do
  tttmessages <- parseAllTicTacToeMessages message
  boardSpaces <- boardSpaceCreateFromTicTacToeMessage tttmessages
  let gameOver = checkIfGameIsOver boardSpaces
  let in if length boardSpaces == 9
    then Left "Game Over. Result: Tie"
    else if gameOver
      then Left "Game Over. Result: Lost"
      else do
        let nextMove = chooseNextMove boardSpaces
        return $ createBencodeMessageString (boardSpaceGetCoordinatesOfSpace nextMove) (boardSpaceGetSign nextMove) playerName message

chooseNextMove :: [BoardSpace] -> BoardSpace
chooseNextMove listOfOccupiedSpaces =
  if length winningMoves > 0
    then head winningMoves
    else if isJust attackMove
      then fromJust attackMove
      else if length defenceMoves > 0
        then head defenceMoves
        else head (boardSpaceGenerateAllPossibleMoves listOfOccupiedSpaces)
  where
    winningMoves = winningConditionCheckBoard listOfOccupiedSpaces
    attackMove = outrightWin listOfOccupiedSpaces
    defenceMoves = findADefenciveMove listOfOccupiedSpaces

-- Functions to make a winning move (if available)
winningConditionCheckBoard :: [BoardSpace] -> [BoardSpace]
winningConditionCheckBoard listOfBoardSpaces =  listRemoveDuplicates $ concat[victoryRows,victoryColumns,victoryDiagonals]
  where
    victoryRows = winningConditionCheckRows listOfBoardSpaces
    victoryColumns = winningConditionCheckColumns listOfBoardSpaces
    victoryDiagonals = winningConditionCheckDiagonals listOfBoardSpaces

winningConditionCheckRows :: [BoardSpace] -> [BoardSpace]
winningConditionCheckRows spaces = winningConditionCheckLines spaces boardSpaceGetRow False True

winningConditionCheckColumns :: [BoardSpace] -> [BoardSpace]
winningConditionCheckColumns spaces = winningConditionCheckLines spaces boardSpaceGetColumn True False

winningConditionCheckDiagonals :: [BoardSpace] -> [BoardSpace]
winningConditionCheckDiagonals spaces = winningConditionCheckLines spaces boardSpaceGetDiagonal False False

winningConditionCheckLines :: [BoardSpace] -> ([BoardSpace] -> Int -> [BoardSpace]) -> Bool -> Bool -> [BoardSpace]
--function takes current tictactoe board , filtering function to filter board by rows , columns or diagonals,
--two boolean values that say what kind of lines function processes now (F T = Rows , T F = Columns , F F = Diagonals)
--function returns list of moves of which all grant you victory (if possible)
winningConditionCheckLines listOfBoardSpaces filteringFunction isXConstantInThisLine isYConstantInThisLine
  = catMaybes $ map wrap boardLines
  where
    getBoardLine :: Int -> [BoardSpace]
    getBoardLine lineNumber = filteringFunction listOfBoardSpaces lineNumber
    boardLines = map getBoardLine [0,1,2]
    wrap :: [BoardSpace] -> Maybe BoardSpace
    wrap boardLine = winningConditionCheckGivenLine boardLine
    winningConditionCheckGivenLine :: [BoardSpace] -> Maybe BoardSpace
    winningConditionCheckGivenLine lineSpaces =
      if length lineSpaces == 2 && boardSpaceAreSignsEqual lineSpaces
        then Just $ BoardSpace xCoord yCoord (boardSpaceGetSign (head lineSpaces))
        else Nothing
      where
        xCoord =
          if isXConstantInThisLine
            then boardSpaceGetX (head lineSpaces)
            else head ([0,1,2] \\ (map x lineSpaces))
        yCoord =
          if isYConstantInThisLine
            then boardSpaceGetY (head lineSpaces)
            else head ([0,1,2] \\ (map y lineSpaces))

--defencive moves are found by eliminating all moves that bring instant defeat from pool of all possible moves
findADefenciveMove :: [BoardSpace] -> [BoardSpace]
findADefenciveMove listOfBoardSpaces = allPossibleMoves \\ defenceGetAllLosingMoves listOfBoardSpaces
  where
    allPossibleMoves = boardSpaceGenerateAllPossibleMoves listOfBoardSpaces

defenceGetAllLosingMoves :: [BoardSpace] -> [BoardSpace]
defenceGetAllLosingMoves boardSpaces = concat [losingMovesInRows,losingMovesInColumns,losingMovesInDiagonals]
  where
    losingMovesInRows = defenceCheckLosingMovesForLines boardSpaces defenceCheckLosingMovesForRow
    losingMovesInColumns = defenceCheckLosingMovesForLines boardSpaces defenceCheckLosingMovesForColumn
    losingMovesInDiagonals = defenceCheckLosingMovesForLines boardSpaces defenceCheckLosingMovesForDiagonal

defenceCheckLosingMovesForLines :: [BoardSpace] -> ([BoardSpace] -> Int -> [BoardSpace]) -> [BoardSpace]
defenceCheckLosingMovesForLines boardSpaces checkFunction = concat $ map wrap [0,1,2]
  where
    wrap :: Int -> [BoardSpace]
    wrap number = checkFunction boardSpaces number

defenceCheckLosingMovesForRow :: [BoardSpace] -> Int -> [BoardSpace]
defenceCheckLosingMovesForRow listOfBoardSpaces rowNumber =
  if length filteredList == 1
    then [(BoardSpace (head xCoords) yCoord char),(BoardSpace (head(tail xCoords)) yCoord char)]
    else []
  where
    filteredList = boardSpaceGetRow listOfBoardSpaces rowNumber
    xCoords = [0,1,2] \\ (map x filteredList)
    yCoord = y (head filteredList)
    char = s (head filteredList)

defenceCheckLosingMovesForColumn :: [BoardSpace] -> Int -> [BoardSpace]
defenceCheckLosingMovesForColumn listOfBoardSpaces columnNumber =
  if length filteredList == 1
    then [(BoardSpace xCoord (head yCoords) char),(BoardSpace xCoord (head(tail yCoords)) char)]
    else []
  where
    filteredList = boardSpaceGetColumn listOfBoardSpaces columnNumber
    xCoord = x (head filteredList)
    yCoords = [0,1,2] \\ (map y filteredList)
    char = s (head filteredList)

defenceCheckLosingMovesForDiagonal :: [BoardSpace] -> Int -> [BoardSpace]
defenceCheckLosingMovesForDiagonal listOfBoardSpaces columnNumber =
  if length filteredList == 1
    then [(BoardSpace (head xCoords) (head yCoords) char),(BoardSpace (head(tail xCoords))  (head(tail yCoords)) char)]
    else []
  where
    filteredList = boardSpaceGetDiagonal listOfBoardSpaces columnNumber
    xCoords = [0,1,2] \\ (map x filteredList)
    yCoords = if columnNumber == 1
      then [0,1,2] \\ (map y filteredList)
      else [2,1,0] \\ (map y filteredList)
    char = s (head filteredList)

-- special case - 100% winrate attack function
outrightWin :: [BoardSpace] -> Maybe BoardSpace
outrightWin [] = Just $ BoardSpace 1 1 'o'
outrightWin listOfBoardSpaces
  | odd (length listOfBoardSpaces) = Nothing
  | isCenterOccupied && odd (edgesOccupied) && cornersOccupied == 0 = Just $ getRequiredEdge occupiedEdges
  | edgesOccupied == 0 && isCenterOccupied && cornersOccupied == 1 = Just $ fromJust $ boardSpaceOppositeCorner (head occupiedCorners)
  | otherwise = Nothing
  where
    occupiedEdges = boardSpaceGetAllOccupiedEdges listOfBoardSpaces
    edgesOccupied = length occupiedEdges
    occupiedCorners = boardSpaceGetAllOccupiedCorners listOfBoardSpaces
    cornersOccupied = length occupiedCorners
    isCenterOccupied = length (boardSpaceGetOccupiedCenter listOfBoardSpaces) == 1
    getRequiredEdge :: [BoardSpace] -> BoardSpace
    getRequiredEdge boardSpaces =
      if length occupiedEdges == 1
        then fromJust $ boardSpaceOppositeEdge (head occupiedEdges)
        else head $ (generateBoardSpaces boardEdges [boardSpaceGetSign (head occupiedEdges)]) \\ occupiedEdges

checkIfGameIsOver :: [BoardSpace] -> Bool
checkIfGameIsOver boardSpaces = or (concat[gameOverInRows,gameOverInColumns,gameOverInDiagonals])
  where
    gameOverInRows = checkIfGameIsOverLines boardSpaces boardSpaceGetRow
    gameOverInColumns = checkIfGameIsOverLines boardSpaces boardSpaceGetColumn
    gameOverInDiagonals = checkIfGameIsOverLines boardSpaces boardSpaceGetDiagonal

checkIfGameIsOverLines :: [BoardSpace] -> ([BoardSpace] -> Int -> [BoardSpace]) -> [Bool]
checkIfGameIsOverLines boardSpaces filterFuntcion = map checkIfGameIsOverLine boardSpacesLines
  where
    wrap :: Int -> [BoardSpace]
    wrap number = filterFuntcion boardSpaces number
    boardSpacesLines = map wrap [0,1,2]

checkIfGameIsOverLine :: [BoardSpace] -> Bool
checkIfGameIsOverLine listOfBoardSpaces =
  if length listOfBoardSpaces /= 3
    then False
    else boardSpaceAreSignsEqual listOfBoardSpaces
