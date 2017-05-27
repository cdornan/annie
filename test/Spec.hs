{-# LANGUAGE OverloadedStrings #-}

import           Annie.Mapper
import qualified Data.ByteString.Char8      as B
import           Test.Hspec


main :: IO ()
main = hspec $ do
    describe "namesHighlighter" $ do
        it "can parse plain strings" $ do
            searchAT byteStringAnnie namesHighlighter "Hello There" `shouldBe` (["Hello There"] :: [B.ByteString])
        it "can parse UTF-8 strings" $ do
            searchAT byteStringAnnie namesHighlighter "‘I’ll tell it her,’ said the Mock Turtle in a deep, hollow tone: ‘sit" `shouldBe` (["Mock Turtle"] :: [B.ByteString])
    describe "phoneNumberHighlighter" $ do
        it "can parse flat number" $ do
            searchAT byteStringAnnie phoneNumberHighlighter " 0123345678asd " `shouldBe` (["0123345678"] :: [B.ByteString])
        it "can parse plus notation" $ do
            searchAT byteStringAnnie phoneNumberHighlighter "+31123345678asd" `shouldBe` (["+31123345678"] :: [B.ByteString])
