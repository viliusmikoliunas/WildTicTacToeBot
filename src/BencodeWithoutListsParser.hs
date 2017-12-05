module BencodeWithoutListsParser(createBencodeMessageString
                                ,parseAllTicTacToeMessages
                                ,TicTacToeMessage(..)) where

import Data.Char (isDigit)

data TicTacToeMessage = Message {   xy :: (Int,Int)
                                , sign :: Char
                                , name :: String
                                } deriving (Show)

createBencodeMessageString :: (Int,Int) -> Char -> String -> String -> String
createBencodeMessageString coordinates sign playerName oldMessage =
  concat ["d1:cd1:0i",show (fst coordinates),"e1:1i",show (snd coordinates),"ee",
          "2:id",show nameLength,":",playerName,oldMessageWithAppends,"1:v1:",[sign],"e"]
  where
    nameLength = length playerName
    oldMessageWithAppends =
      if length oldMessage == 0 || oldMessage == "de"
        then ""
        else "4:prev" ++ oldMessage

parseAllTicTacToeMessages :: String -> Either String [TicTacToeMessage]
parseAllTicTacToeMessages ('d':'e':rest) = Right []
parseAllTicTacToeMessages msg =
  case parsedMessage of
    Left parsedMessage -> Left parsedMessage
    Right parsedMessage -> Right (fst parsedMessage)
  where
    parseAllTicTacToeMessages' :: Either String ([TicTacToeMessage],String) -> Either String ([TicTacToeMessage],String)
    parseAllTicTacToeMessages' (Left errmsg) = Left errmsg
    parseAllTicTacToeMessages' (Right (tttmessages,rest))
      | length rest == 0 = Right (reverse tttmessages, "")
      | head rest == 'e' = Right (reverse tttmessages, "")
      | otherwise =
      let
        parsedMsg = parseTicTacToeBencodeMessage rest
        answer =
          case parsedMsg of
            Left parsedMsg -> Left parsedMsg
            Right parsedMsg -> Right ((fst parsedMsg):tttmessages,snd parsedMsg)
        in parseAllTicTacToeMessages' answer
    parsedMessage = parseAllTicTacToeMessages' (Right ([],msg))

parseTicTacToeBencodeMessage :: String -> Either String (TicTacToeMessage,String)
parseTicTacToeBencodeMessage ('d':rest) =
  case cmessage of
    Left cmessage -> Left cmessage
    Right cmessage -> case idmessage of
      Left idmessage -> Left idmessage
      Right idmessage -> case vmessage of
        Left vmessage -> Left vmessage
        Right vmessage ->
          let
            tttmsg = Message (fst cmessage) (head(fst vmessage)) (fst idmessage)
            in Right (tttmsg,snd vmessage)
  where
    msg = init rest
    cmessage = parseTicTacToeCmessage msg--validate
    idmessage =
      case cmessage of
        Left cmessage -> Left cmessage
        Right cmessage -> parseTicTacToeIdMessage (snd cmessage)
    vmessage =
      case idmessage of
        Left idmessage -> Left idmessage
        Right idmessage -> parseTicTacToeVmessage (snd idmessage)
parseTicTacToeBencodeMessage _ = Left "Bad TicTacToeMessage format"

parseTicTacToeVmessage :: String -> Either String (String,String)
parseTicTacToeVmessage ('4':':':'p':'r':'e':'v':rest) = getPreviousMessageWithCurrentSign rest
parseTicTacToeVmessage ('1':':':'v':rest) = parseBencodeString rest
parseTicTacToeVmessage _ = Left "Bad v message format"

getPreviousMessageWithCurrentSign :: String -> Either String (String,String)
getPreviousMessageWithCurrentSign prevMsgWithSign =
  let
    signPart = reverse (takeWhile (/= 'e') (reverse prevMsgWithSign))
    prevMsg = take (length prevMsgWithSign - length signPart) prevMsgWithSign
    sign = parseTicTacToeVmessage signPart
    in case sign of
      Left sign -> Left sign
      Right sign -> Right (fst sign, prevMsg)

parseTicTacToeIdMessage :: String -> Either String (String,String)
parseTicTacToeIdMessage ('2':':':'i':'d':rest) = parseBencodeString rest
parseTicTacToeIdMessage _ = Left "Wrong id message format"

parseTicTacToeCmessage :: String -> Either String ((Int,Int),String)
parseTicTacToeCmessage ('1':':':'c':message) = parseTicTacToeCoordinatess message
parseTicTacToeCmessage _ = Left "Wrong c message format"

parseTicTacToeCoordinatess :: String -> Either String ((Int,Int),String)
parseTicTacToeCoordinatess ('d':msg) =
  case x of
    Left x -> Left x
    Right x -> case y of
      Left y -> Left y
      Right y -> case left of
        Left left -> Left left
        Right left -> Right ((fst x, fst y),left)

  where
    signx = parseBencodeString msg
    x =
      case signx of
        Left signx -> Left signx
        Right signx -> parseBencodeInteger $ snd signx
    signy =
      case x of
        Left x -> Left x
        Right x -> parseBencodeString $ snd x
    y =
      case signy of
        Left signy -> Left signy
        Right signy -> parseBencodeInteger $ snd signy
    left =
      case y of
        Left y -> Left y
        Right y -> let
          dictEndChar = head (snd y)
          in if dictEndChar == 'e'
            then Right (drop 1 (snd y))
            else Left "Coordinate dictionary wrong end char"

parseTicTacToeC _ = Left "Bad coordinate format"

parseBencodeInteger :: String -> Either String (Int, String)
parseBencodeInteger ('i':rest)
  | length intAsStr == length rest = Left "End of bencode integer not found"
  | not (checkIfInteger intAsStr) = Left "Non digit symbols found in Bencode integer"
  | otherwise = Right ((read intAsStr :: Int), rest1)
  where
    intAsStr = takeWhile (/= 'e') rest
    rest1 = drop (length intAsStr + 1) rest
parseBencodeInteger _ = Left "Bad bencode integer format"

checkIfInteger :: String -> Bool
checkIfInteger str =
  if length tempStr == length str
    then True
    else False
  where
    tempStr = takeWhile (isDigit) str

parseBencodeString :: String -> Either String (String,String)
parseBencodeString str
  | length str < 3 = Left "Bad bencode string format"
  | length bencodeStringLengthAsString == 0 = Left "String doesnt start with number"
  | charAfterDigits /= ":" = Left "Bad bencode string format"
  | stringLength > length stringWithoutLengthPrefix = Left "Bencode string too short"
  | not (checkIfNextSymbolBelongsToBencodeType charAfterBencodeString) = Left "Bencode format broken"
  | otherwise = Right (take stringLength stringWithoutLengthPrefix, drop stringLength stringWithoutLengthPrefix)
  where
    bencodeStringLengthAsString = takeWhile (isDigit) str
    stringLength = read bencodeStringLengthAsString :: Int
    charAfterDigits = take 1 (drop (length bencodeStringLengthAsString) str)
    stringWithoutLengthPrefix = drop (length bencodeStringLengthAsString + 1) str
    charAfterBencodeString = take 1 (drop stringLength stringWithoutLengthPrefix)

checkIfNextSymbolBelongsToBencodeType :: String -> Bool
checkIfNextSymbolBelongsToBencodeType c =
  if length c > 1
    then False
    else if length c == 0
      then True
      else if isDigit $ head c
        then True
        else elem (head c) "idle"
