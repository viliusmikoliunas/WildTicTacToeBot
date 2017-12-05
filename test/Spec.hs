import PlayAlgorithm
import DataBoardSpace

import Test.Hspec
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "Check if chooseNextMove makes a winning move" $ do
      (chooseNextMove [BoardSpace 0 0 'x' , BoardSpace 1 1 'x']) `shouldBe` (BoardSpace 2 2 'x')
