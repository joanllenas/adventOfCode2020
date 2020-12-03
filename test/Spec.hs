import Day2 (PasswordDef (MkPasswordDef), parsePasswordDef)
import Day3 (JourneyLog (..), TerrainItem (..), addSlope, getItemAtCoord)
import Test.Hspec
import Test.QuickCheck
import qualified Text.Parsec as P

main :: IO ()
main = hspec $ do
  describe "Day2" $ do
    it "parsing '1-3 a: abcde' should be Right" $ do
      parsePasswordDef "1-3 a: bcade" `shouldBe` Right (MkPasswordDef 1 3 'a' "bcade")
  describe "Day3" $ do
    it "getItemAtCoord (0,0)" $ do
      getItemAtCoord [[Tree, OpenSquare, OpenSquare]] (0, 0) `shouldBe` Just Tree
    it "getItemAtCoord (1,0)" $ do
      getItemAtCoord [[OpenSquare, Tree, OpenSquare]] (1, 0) `shouldBe` Just Tree
    it "getItemAtCoord (2,0)" $ do
      getItemAtCoord [[OpenSquare, OpenSquare, Tree]] (2, 0) `shouldBe` Just Tree
    it "getItemAtCoord (3,0)" $ do
      getItemAtCoord [[Tree, OpenSquare, OpenSquare]] (3, 0) `shouldBe` Just Tree
    it "getItemAtCoord (3,1)" $ do
      getItemAtCoord [[OpenSquare, OpenSquare, OpenSquare], [Tree, OpenSquare, OpenSquare]] (3, 1) `shouldBe` Just Tree
