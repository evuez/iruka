{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (encode)
import Lib (Card(..), parser)
import Test.Hspec
import Test.Hspec.Megaparsec (shouldParse)

main :: IO ()
main =
  hspec $ do
    describe "parser" $ do
      it "parses a well-formed card with no extra fields" $
        parser
          "term: Term\n\
          \---\n\
          \A\n\
          \B\n\
          \---\n" `shouldParse`
        [Card "Term" "A\nB" [] Nothing Nothing Nothing]
      it "parses a well-formed card with extra fields" $
        parser
          "term: Term\n\
          \tags: Tag 1, Tag 2\n\
          \image: image-uri\n\
          \color: #000\n\
          \link: https://example.org\n\
          \---\n\
          \A\n\
          \B\n\
          \---\n" `shouldParse`
        [ Card
            "Term"
            "A\nB"
            ["Tag 1", "Tag 2"]
            (Just "image-uri")
            (Just "#000")
            (Just "https://example.org")
        ]
      it "parses a well-formed card with an empty definition" $
        parser
          "term: Term\n\
          \---\n\
          \---\n" `shouldParse`
        [Card "Term" "" [] Nothing Nothing Nothing]
      it "doesn't care about extra fields ordering" $
        parser
          "term: Term\n\
          \color: #000\n\
          \image: image-uri\n\
          \link: https://example.org\n\
          \tags: Tag 1, Tag 2\n\
          \---\n\
          \A\n\
          \B\n\
          \---\n" `shouldParse`
        [ Card
            "Term"
            "A\nB"
            ["Tag 1", "Tag 2"]
            (Just "image-uri")
            (Just "#000")
            (Just "https://example.org")
        ]
      it "doesn't care about missing extra fields" $
        parser
          "term: Term\n\
          \color: #000\n\
          \tags: Tag 1, Tag 2\n\
          \---\n\
          \A\n\
          \B\n\
          \---\n" `shouldParse`
        [Card "Term" "A\nB" ["Tag 1", "Tag 2"] Nothing (Just "#000") Nothing]
      it "parses a list of cards" $
        parser
          "term: Term 1\n\
          \---\n\
          \A\n\
          \---\n\
          \term: Term 2\n\
          \---\n\
          \B\n\
          \---\n" `shouldParse`
        [ Card "Term 1" "A" [] Nothing Nothing Nothing
        , Card "Term 2" "B" [] Nothing Nothing Nothing
        ]
    describe "encoder" $ do
      it "encodes a card" $
        encode
          (Card
             "Term 1"
             "A"
             ["1", "2"]
             (Just "image-uri")
             (Just "#000")
             (Just "https://example.org")) `shouldBe`
        "{\"image\":\"image-uri\",\"color\":\"#000\",\"definition\":\"A\",\"link\":\"https://example.org\",\"term\":\"Term 1\",\"tags\":[\"1\",\"2\"]}"
      it "encodes a list of cards" $
        encode
          [ Card
              "Term 1"
              "A"
              ["1", "2"]
              (Just "image-uri")
              (Just "#000")
              Nothing
          ] `shouldBe`
        "[{\"image\":\"image-uri\",\"color\":\"#000\",\"definition\":\"A\",\"link\":null,\"term\":\"Term 1\",\"tags\":[\"1\",\"2\"]}]"
