module Main where

import Lib
import Network.HTTP
import PlayAlgorithm(getNextMoveMessage)

main :: IO()
main = attack ""
-- attack "" or defend

theGETrequest :: String -> Request String
theGETrequest playerNumber = Request { rqURI     = (parsedServerURL playerNumber)
                                     , rqMethod  = GET
                                     , rqHeaders = [acceptHeader]
                                     , rqBody    = ""
                                     }
  where
    acceptHeader = mkHeader HdrAccept contentType

createPOSTrequest :: String -> String -> Request String
createPOSTrequest playerNumber msg = Request { rqURI     = (parsedServerURL playerNumber)
                                             , rqMethod  = POST
                                             , rqHeaders = [contentTypeHeader,contentLengthHeader]
                                             , rqBody    = msg
                                             }
  where
    contentTypeHeader = mkHeader HdrContentType contentType
    contentLengthHeader = mkHeader HdrContentLength (show(length msg))


attack :: String -> IO()
attack message = do
  let newMessage = getNextMoveMessage message playerName
  let in case newMessage of
    Left newMessage -> putStr newMessage
    Right newMessage -> do
      let postRequest = createPOSTrequest "1" newMessage
      httpResponse <- simpleHTTP postRequest
      let in case httpResponse of
        Left httpResponse -> putStr "POST error"
        Right httpResponse -> do
          let in if (checkIfGameIsOverBMessage newMessage)
            then putStr "Game over. Result: Win"
            else do
              getRequest <- simpleHTTP $ theGETrequest "1"
              let in case getRequest of
                Left getRequest -> putStr "GET error"
                Right getRequest -> attack $ rspBody getRequest


defend :: IO()
defend = do
  getRequest <- simpleHTTP $ theGETrequest "2"
  let in case getRequest of
    Left getRequest -> putStr "GET error"
    Right getRequest -> do
      let newMessage = getNextMoveMessage (rspBody getRequest) playerName
      let in case newMessage of
        Left newMessage -> putStr newMessage
        Right newMessage -> do
          let postRequest = createPOSTrequest "2" newMessage
          httpResponse <- simpleHTTP postRequest
          let in case httpResponse of
            Left httpResponse -> putStr "POST error"
            Right httpResponse -> do --iterpti Win checka
              let in if (checkIfGameIsOverBMessage newMessage)
                then putStr "Game over. Result: Win"
                else do defend
