module Models.ParseFailureSpec where

import           BasicPrelude
import           Data.Aeson                     ( Value(Number, String)
                                                , decode
                                                , encode
                                                , toJSON
                                                )
import qualified Data.Aeson.KeyMap             as KM
import qualified Data.Text                     as T
import           Test.Hspec

import           Models.ParseFailure
import           Util                           ( (|>) )


spec :: SpecWith ()
spec = parallel $ do
  describe "parseFailure" $ do
    it "builds a failure with only source file and message" $ do
      let f = parseFailure "NRS-001.html" "missing section body"
      sourceFile f `shouldBe` "NRS-001.html"
      message f `shouldBe` "missing section body"
      titleNumber f `shouldBe` Nothing
      chapterNumber f `shouldBe` Nothing
      sectionNumber f `shouldBe` Nothing

    it "attaches title, chapter, and section location" $ do
      let f =
            parseFailure "NRS-001.html" "bad number"
              |> atTitle 1
              |> atChapter "1"
              |> atSection "1.010"
      titleNumber f `shouldBe` Just 1
      chapterNumber f `shouldBe` Just "1"
      sectionNumber f `shouldBe` Just "1.010"

  describe "ToJSON" $ do
    it "emits the location and message fields" $ do
      let f =
            parseFailure "chap.html" "could not parse"
              |> atTitle 3
              |> atChapter "10"
              |> atSection "10.020"
          decoded = decode (encode f) :: Maybe (KM.KeyMap Value)
      case decoded of
        Nothing  -> expectationFailure "ToJSON output did not decode"
        Just obj -> do
          obj KM.!? "sourceFile" `shouldBe` Just (String "chap.html")
          obj KM.!? "titleNumber" `shouldBe` Just (Number 3)
          obj KM.!? "chapterNumber" `shouldBe` Just (String "10")
          obj KM.!? "sectionNumber" `shouldBe` Just (String "10.020")
          obj KM.!? "message" `shouldBe` Just (String "could not parse")
          toJSON f `shouldBe` toJSON obj

  describe "accumulate" $ do
    it "keeps Rights and turns Lefts into located failures" $ do
      let inputs = ["ok-a", "bad-1", "ok-b", "bad-2"] :: [Text]
          parse s =
            if T.isPrefixOf "bad" s then Left "boom" else Right (s <> "!")
          toFailure s err =
            parseFailure "file.html" (T.pack err) |> atSection s
          (oks, fails) = accumulate parse toFailure inputs
      oks `shouldBe` ["ok-a!", "ok-b!"]
      length fails `shouldBe` 2
      message (fails !! 0) `shouldBe` "boom"
      sectionNumber (fails !! 0) `shouldBe` Just "bad-1"
      message (fails !! 1) `shouldBe` "boom"
      sectionNumber (fails !! 1) `shouldBe` Just "bad-2"
      sourceFile (fails !! 0) `shouldBe` "file.html"

    it "returns empty lists when given an empty input" $ do
      accumulate (Right :: Int -> Either String Int) (\_ _ -> parseFailure "x" "y") []
        `shouldBe` ([], [])

    it "returns only failures when every item fails" $ do
      let (oks, fails) =
            accumulate
              (const (Left "nope") :: Text -> Either String Text)
              (\a e -> parseFailure a (T.pack e))
              ["a", "b"]
      oks `shouldBe` []
      map sourceFile fails `shouldBe` ["a", "b"]
      map message fails `shouldBe` ["nope", "nope"]


main :: IO ()
main = hspec spec
