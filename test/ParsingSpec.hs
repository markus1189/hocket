{-# LANGUAGE OverloadedStrings #-}
module ParsingSpec(main, spec) where

import Test.Hspec
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as M

import Parsing

main :: IO ()
main = hspec spec

exampleItemJSON :: BL.ByteString
exampleItemJSON = CL.pack "{\"excerpt\": \"While there are myriad mocking libraries available for the Java platform, only a select few of these nifty frameworks is capable of mocking the non-mock-friendly modifiers of static and final.\", \"favorite\": \"0\", \"given_title\": \"Sometimes TDD Requires a Hammer\", \"given_url\": \"http://feedproxy.google.com/~r/JavaCodeGeeks/~3/GO70AhJ4s2Y/sometimes-tdd-requires-a-hammer.html\", \"has_image\": \"1\", \"has_video\": \"0\", \"is_article\": \"1\", \"is_index\": \"0\", \"item_id\": \"411175066\", \"resolved_id\": \"411117461\", \"resolved_title\": \"Sometimes TDD Requires a Hammer\", \"resolved_url\": \"http://www.javacodegeeks.com/2013/08/sometimes-tdd-requires-a-hammer.html\", \"sort_id\": 2, \"status\": \"0\", \"time_added\": \"1375807690\", \"time_favorited\": \"0\", \"time_read\": \"0\", \"time_updated\": \"1375807690\", \"word_count\": \"592\"}"

examplePocketItemsJSON :: Maybe (M.Map String PocketItem)
examplePocketItemsJSON = unsafePerformIO $ parseItemsFile "test/example.json"

spec :: Spec
spec = do
  describe "pocket item parsing" $ do
    it "parse a pocket item" $ do
       let Just result = parseItem exampleItemJSON
       wordCount result `shouldBe` 592
    it "should parse the example file" $ do
       let Just parsed = examplePocketItemsJSON
       M.size parsed `shouldBe` 10
