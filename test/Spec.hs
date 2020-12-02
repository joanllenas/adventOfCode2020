import Day2 (PasswordDef (MkPasswordDef), parsePasswordDef)
import Test.Hspec
import Test.QuickCheck
import qualified Text.Parsec as P

main :: IO ()
main = hspec $ do
  describe "Day2" $ do
    it "parsing '1-3 a: abcde' should be Right" $ do
      parsePasswordDef "1-3 a: bcade" `shouldBe` Right (MkPasswordDef 1 3 'a' "bcade")
