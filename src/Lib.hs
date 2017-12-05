module Lib where
import PlayAlgorithm
import DataBoardSpace
import BencodeWithoutListsParser

import Network.URI
import Data.Maybe(fromJust)

someFunc :: IO ()
someFunc = putStr ""

contentType :: String
contentType = "application/bencode+map"

gameTitle :: String
gameTitle = "gameTitle123"

playerName :: String
playerName = "PlayerNumberOne"

serverURL :: String
serverURL = concat["http://tictactoe.haskell.lt/game/",gameTitle,"/player/"] -- ++playerNumberInStr

parsedServerURL :: String -> URI
parsedServerURL nr = fromJust $ parseURI (serverURL++nr)

checkIfGameIsOverBMessage :: String -> Bool
checkIfGameIsOverBMessage bencodeMessage = case tttmessages of
  Left tttmessages -> False
  Right tttmessages -> case boardSpacesE of
    Left boardSpacesE -> False
    Right boardSpacesE -> case boardSpaces of
      Left boardSpaces -> False
      Right boardSpaces -> boardSpaces
  where
    tttmessages = parseAllTicTacToeMessages bencodeMessage
    boardSpacesE = case tttmessages of
      Left tttmessages -> Left tttmessages
      Right tttmessages -> boardSpaceCreateFromTicTacToeMessage tttmessages

    boardSpaces = case boardSpacesE of
      Left boardSpacesE -> Left boardSpacesE
      Right boardSpacesE -> Right $ checkIfGameIsOver boardSpacesE
