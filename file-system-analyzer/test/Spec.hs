module Spec (main) where

import Test.Hspec
import Lib
import qualified Data.Map as Map

kb :: Integer
kb = 1024

spec :: Spec
spec = do
  describe "updateStats" $ do
    it "updates FileStats correctly for a new file" $ do
      let initialStats = emptyStats
      let fileSize = 150
      let extension = ".hs"
      let updatedStats = updateStats fileSize extension initialStats

      updatedStats `shouldBe` FileStats 150 1 (Map.fromList [(".hs", 1)])
      -- show that the original stats are unchanged
      initialStats `shouldBe` emptyStats

    it "updates FileStats correctly with multiple updates" $ do
      let stats1 = updateStats 100 ".txt" emptyStats
      let stats2 = updateStats 200 ".hs" stats1
      let stats3 = updateStats 50 ".txt" stats2

      stats3 `shouldBe` FileStats 350 3 (Map.fromList [(".txt", 2), (".hs", 1)])
      -- show that the original stats are unchanged
      stats1 `shouldBe` FileStats 100 1 (Map.fromList [(".txt", 1)])
      stats2 `shouldBe` FileStats 300 2 (Map.fromList [(".txt", 1), (".hs", 1)])

  describe "combineStats" $ do
    it "combines two FileStats correctly" $ do
      let stats1 = FileStats 100 1 (Map.fromList [(".hs", 1)])
      let stats2 = FileStats 200 2 (Map.fromList [(".txt", 2), (".hs", 1)])
      let combined = combineStats stats1 stats2

      combined `shouldBe` FileStats 300 3 (Map.fromList [(".txt", 2), (".hs", 2)])
      -- show that the original stats are unchanged
      stats1 `shouldBe` FileStats 100 1 (Map.fromList [(".hs", 1)])
      stats2 `shouldBe` FileStats 200 2 (Map.fromList [(".txt", 2), (".hs", 1)])
  
  describe "prettyPrintSize" $ do
    it "prints bytes correctly" $ do
      prettyPrintSize 0 `shouldBe` "0 bytes"
      prettyPrintSize 1 `shouldBe` "1 bytes"
      prettyPrintSize 10 `shouldBe` "10 bytes"
      prettyPrintSize 100 `shouldBe` "100 bytes"
      prettyPrintSize 1000 `shouldBe` "1000 bytes"

    it "prints KB correctly" $ do
      prettyPrintSize kb `shouldBe` "1 KB 0 bytes"
      prettyPrintSize (kb + 1) `shouldBe` "1 KB 1 bytes"
      prettyPrintSize (kb * 10) `shouldBe` "10 KB 0 bytes"
      prettyPrintSize (kb * 100) `shouldBe` "100 KB 0 bytes"
      prettyPrintSize (kb * 1000) `shouldBe` "1000 KB 0 bytes"

    it "prints MB correctly" $ do
      prettyPrintSize (kb * kb) `shouldBe` "1 MB 0 bytes"
      prettyPrintSize (kb * kb + 1) `shouldBe` "1 MB 1 bytes"
      prettyPrintSize (kb * kb * 10) `shouldBe` "10 MB 0 bytes"
      prettyPrintSize (kb * kb * 100) `shouldBe` "100 MB 0 bytes"
      prettyPrintSize (kb * kb * 1000) `shouldBe` "1000 MB 0 bytes"

    it "prints GB correctly" $ do
      prettyPrintSize (kb * kb * kb) `shouldBe` "1 GB 0 bytes"
      prettyPrintSize (kb * kb * kb + 1) `shouldBe` "1 GB 1 bytes"
      prettyPrintSize (kb * kb * kb * 10) `shouldBe` "10 GB 0 bytes"
      prettyPrintSize (kb * kb * kb * 100) `shouldBe` "100 GB 0 bytes"
      prettyPrintSize (kb * kb * kb * 1000) `shouldBe` "1000 GB 0 bytes"

main :: IO ()
main = hspec spec
