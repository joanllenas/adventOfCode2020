import Day2 (PasswordDef (MkPasswordDef), parsePasswordDef)
import Day3 (JourneyLog (..), TerrainItem (..), addSlope, getItemAtCoord)
import Day4 (KeyValue (Kv), hexParser, keyValueParser, passwordHasRequiredKeys)
import Test.Hspec
import Test.QuickCheck
import qualified Utils as U
import qualified Validation as Val

main :: IO ()
main = hspec $ do
  describe "Utils" $ do
    it "hasSuffix 'cm' '1523cm' should be True" $ do
      U.hasSuffix "cm" "1523cm" `shouldBe` True
    it "hasSuffix 'cm' '1cm' should be True" $ do
      U.hasSuffix "cm" "1cm" `shouldBe` True
    it "hasSuffix 'cm' '1in' should be False" $ do
      U.hasSuffix "cm" "1in" `shouldBe` False
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
  describe "Day4" $ do
    it "parsing 'hcl:fffffd' should be Right" $ do
      U.parse keyValueParser "hcl:fffffd" `shouldBe` Right (Kv "hcl" "fffffd")
    it "parsing 'hcl:#fffffd' should be Right" $ do
      U.parse keyValueParser "hcl:#fffffd" `shouldBe` Right (Kv "hcl" "#fffffd")
    it "parsing 'hcl:12345' should be Right" $ do
      U.parse keyValueParser "hcl:12345" `shouldBe` Right (Kv "hcl" "12345")
    it "parsing 'hcl:1a2b' should be Right" $ do
      U.parse keyValueParser "hcl:1a2b" `shouldBe` Right (Kv "hcl" "1a2b")
    it "passwordHasRequiredKeys ['hcl:1a2b'] should be False" $ do
      passwordHasRequiredKeys ["hcl:1a2b"] `shouldBe` False
    it "passwordHasRequiredKeys with all key value pairs should be True" $ do
      passwordHasRequiredKeys ["byr:abcd", "iyr:abcd", "eyr:abcd", "hgt:abcd", "hcl:abcd", "ecl:abcd", "pid:abcd", "cid:abcd"] `shouldBe` True
    it "passwordHasRequiredKeys with all key value pairs except 'byr' should be False" $ do
      passwordHasRequiredKeys ["iyr:abcd", "eyr:abcd", "hgt:abcd", "hcl:abcd", "ecl:abcd", "pid:abcd", "cid:abcd"] `shouldBe` False
    it "passwordHasRequiredKeys with all key value pairs except 'cid' should be True" $ do
      passwordHasRequiredKeys ["byr:abcd", "iyr:abcd", "eyr:abcd", "hgt:abcd", "hcl:abcd", "ecl:abcd", "pid:abcd"] `shouldBe` True
    it "passwordHasRequiredKeys with all key value pairs except 'cid' and 'iyr' should be False" $ do
      passwordHasRequiredKeys ["byr:abcd", "eyr:abcd", "hgt:abcd", "hcl:abcd", "ecl:abcd", "pid:abcd"] `shouldBe` False
    it "parsing '#012def' should be Right" $ do
      U.parse hexParser "#012def" `shouldBe` Right "#012def"
  describe "Validation" $ do
    it "Concat Failures" $ do
      Val.Failure "mal" <> Val.Success "bien" <> Val.Failure "ko" `shouldBe` Val.Failure "malko"
    it "Concat Successes" $ do
      (Val.Success "bien" :: Val.Validation String String) <> (Val.Success "ok" :: Val.Validation String String) `shouldBe` Val.Success "bienok"