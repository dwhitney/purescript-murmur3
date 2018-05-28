module Test.Murmur3.Spec where

import Prelude

import Data.BigInt (BigInt, fromInt)
import Data.BigInt as BigInt
import Data.Maybe (fromJust)
import Murmur3 (hashString)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

hashFn :: String -> BigInt
hashFn = hashString $ fromInt 63476

utf16 :: forall eff. String -> BigInt -> Spec eff Unit
utf16 input expected =
  it input $ expected `shouldEqual` hashString (fromInt 1234) input

fromString :: String -> BigInt
fromString = unsafePartial $ fromJust <<< BigInt.fromString

spec :: forall r. Spec r Unit
spec = describe "Murmur3" do
  describe "Hashing" do
    it "int" $ fromInt 1992578978 `shouldEqual` hashFn "-102433675"
    it "float" $ fromInt 335970363 `shouldEqual` hashFn "4.32"
    it "rec" $ fromString "3455049611" `shouldEqual` hashFn "{ name = \"Robin\", age = \"27\", male = True }"
    it "tuple" $ fromInt 12752532 `shouldEqual` hashFn "(\"Robin\",27,True)"
    it "ls" $ fromString "4202619459" `shouldEqual` hashFn "[1,2,3,4,5,6]"
    it "bool" $ fromInt 108766572 `shouldEqual` hashFn "False"

  describe "UTF-16 strings" do
    utf16 "Turn me into a hash" $ fromString "4138100590"
    utf16 "✓ à la mode" $ fromInt 146308576
    utf16 "💩💩💩" $ fromString "4037155920"
