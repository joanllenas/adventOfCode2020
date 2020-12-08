import Data.Either (isLeft, isRight)
import Day2 (PasswordDef (MkPasswordDef), parsePasswordDef)
import Day3 (JourneyLog (..), TerrainItem (..), addSlope, getItemAtCoord)
import Day4 (KeyValue (Kv), Password, hexParser, keyValueParser, rightKeyValues, validateKey, validatePassword, validatePasswordKeys, validatePasswordRequiredKeys)
import Test.Hspec
import Test.QuickCheck
import qualified Utils as U
import qualified Validation as Val

type Day4Validation = Val.Validation String KeyValue

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

  describe "Validation lib" $ do
    it "Concat Failures" $ do
      Val.Failure "mal" <> Val.Success "bien" <> Val.Failure "ko" `shouldBe` Val.Failure "malko"
    it "Concat Successes" $ do
      (Val.Success "bien" :: Val.Validation String String) <> (Val.Success "ok" :: Val.Validation String String) `shouldBe` Val.Success "bienok"

  describe "Day4" $ do
    it "parsing 'hcl:fffffd' should be Right" $ do
      U.parse keyValueParser "hcl:fffffd" `shouldBe` Right (Kv "hcl" "fffffd")
    it "parsing 'hcl:#fffffd' should be Right" $ do
      U.parse keyValueParser "hcl:#fffffd" `shouldBe` Right (Kv "hcl" "#fffffd")
    it "parsing 'hcl:12345' should be Right" $ do
      U.parse keyValueParser "hcl:12345" `shouldBe` Right (Kv "hcl" "12345")
    it "parsing 'hcl:1a2b' should be Right" $ do
      U.parse keyValueParser "hcl:1a2b" `shouldBe` Right (Kv "hcl" "1a2b")

    it "validatePasswordRequiredKeys ['hcl:1a2b'] should be Left" $ do
      isLeft (validatePasswordRequiredKeys $ rightKeyValues ["hcl:1a2b"]) `shouldBe` True
    it "validatePasswordRequiredKeys with all key value pairs should be Right" $ do
      isRight (validatePasswordRequiredKeys $ rightKeyValues ["byr:abcd", "iyr:abcd", "eyr:abcd", "hgt:abcd", "hcl:abcd", "ecl:abcd", "pid:abcd", "cid:abcd"]) `shouldBe` True
    it "validatePasswordRequiredKeys with all key value pairs except 'byr' should be Left" $ do
      isLeft (validatePasswordRequiredKeys $ rightKeyValues ["iyr:abcd", "eyr:abcd", "hgt:abcd", "hcl:abcd", "ecl:abcd", "pid:abcd", "cid:abcd"]) `shouldBe` True
    it "validatePasswordRequiredKeys with all key value pairs except 'cid' should be Right" $ do
      isRight (validatePasswordRequiredKeys $ rightKeyValues ["byr:abcd", "iyr:abcd", "eyr:abcd", "hgt:abcd", "hcl:abcd", "ecl:abcd", "pid:abcd"]) `shouldBe` True
    it "validatePasswordRequiredKeys with all key value pairs except 'cid' and 'iyr' should be Left" $ do
      isLeft (validatePasswordRequiredKeys $ rightKeyValues ["byr:abcd", "eyr:abcd", "hgt:abcd", "hcl:abcd", "ecl:abcd", "pid:abcd"]) `shouldBe` True
    it "parsing '#012def' should be Right" $ do
      U.parse hexParser "#012def" `shouldBe` Right "#012def"
    it "parsing '#012' should be Left" $ do
      isLeft (U.parse hexParser "#012") `shouldBe` True

    it "validateKey Kv 'byr' '2002' should be Success" $ do
      validateKey (Kv "byr" "2002") `shouldBe` (Val.Success (Kv "byr" "2002") :: Day4Validation)
    it "validateKey Kv 'byr' '2003' should be Failure" $ do
      validateKey (Kv "byr" "2003") `shouldBe` (Val.Failure "Number 2003 out of bounds." :: Day4Validation)

    it "validateKey Kv 'hgt' '60in' should be Success" $ do
      validateKey (Kv "hgt" "60in") `shouldBe` (Val.Success (Kv "hgt" "60in") :: Day4Validation)
    it "validateKey Kv 'hgt' '190in' should be Failure" $ do
      validateKey (Kv "hgt" "190in") `shouldBe` (Val.Failure "Number 190 out of bounds." :: Day4Validation)
    it "validateKey Kv 'hgt' '190cm' should be Success" $ do
      validateKey (Kv "hgt" "190cm") `shouldBe` (Val.Success (Kv "hgt" "190cm") :: Day4Validation)
    it "validateKey Kv 'hgt' '190' should be Failure" $ do
      validateKey (Kv "hgt" "190") `shouldBe` (Val.Failure "Could not parse height: 190." :: Day4Validation)

    it "validateKey Kv 'hcl' '#123abc' should be Success" $ do
      validateKey (Kv "hcl" "#123abc") `shouldBe` (Val.Success (Kv "hcl" "#123abc") :: Day4Validation)
    it "validateKey Kv 'hcl' '#123abz' should be Failure" $ do
      validateKey (Kv "hcl" "#123abz") `shouldBe` (Val.Failure "Could not parse hex: #123abz." :: Day4Validation)
    it "validateKey Kv 'hcl' '123abc' should be Failure" $ do
      validateKey (Kv "hcl" "123abc") `shouldBe` (Val.Failure "Could not parse hex: 123abc." :: Day4Validation)

    it "validateKey Kv 'ecl' 'brn' should be Success" $ do
      validateKey (Kv "ecl" "brn") `shouldBe` (Val.Success (Kv "ecl" "brn") :: Day4Validation)
    it "validateKey Kv 'ecl' 'wat' should be Failure" $ do
      validateKey (Kv "ecl" "wat") `shouldBe` (Val.Failure "Invalid color: wat." :: Day4Validation)

    it "validateKey Kv 'pid' '000000001' should be Success" $ do
      validateKey (Kv "pid" "000000001") `shouldBe` (Val.Success (Kv "pid" "000000001") :: Day4Validation)
    it "validateKey Kv 'pid' '0123456789' should be Failure" $ do
      validateKey (Kv "pid" "0123456789") `shouldBe` (Val.Failure "Invalid pid: 0123456789." :: Day4Validation)

    it "validatePassword should be Failure" $ do
      validatePassword [Kv "eyr" "1972", Kv "cid" "100"] `shouldBe` (Val.Failure "Invalid password: Number 1972 out of bounds." :: Val.Validation String Password)
    it "validatePassword should be Success" $ do
      let pass = [Kv "pid" "087499704", Kv "hgt" "74in", Kv "ecl" "grn", Kv "iyr" "2012", Kv "eyr" "2030", Kv "byr" "1980", Kv "hcl" "#623a2f"]
      validatePassword pass `shouldBe` (Val.Success pass :: Val.Validation String Password)

    it "validatePasswordKeys should be Right" $ do
      let pass = [Kv "pid" "087499704", Kv "hgt" "74in", Kv "ecl" "grn", Kv "iyr" "2012", Kv "eyr" "2030", Kv "byr" "1980", Kv "hcl" "#623a2f"]
      validatePasswordKeys (Right pass) `shouldBe` Right pass
    it "validatePasswordKeys should be Left" $ do
      let pass = [Kv "pid" "087499704", Kv "hgt" "190in", Kv "ecl" "grn", Kv "iyr" "2012", Kv "eyr" "2030", Kv "byr" "1980", Kv "hcl" "#623a2f"]
      validatePasswordKeys (Right pass) `shouldBe` Left "Invalid password: Number 190 out of bounds."
    it "validatePasswordKeys should be Left" $ do
      let pass = [Kv "pid" "087499704", Kv "hgt" "190in", Kv "ecl" "wat", Kv "iyr" "2012", Kv "eyr" "2030", Kv "byr" "1980", Kv "hcl" "#623a2f"]
      validatePasswordKeys (Right pass) `shouldBe` Left "Invalid password: Number 190 out of bounds.Invalid color: wat."
