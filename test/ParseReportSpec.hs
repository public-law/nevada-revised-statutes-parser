{-# OPTIONS_GHC -Wno-x-partial #-}
module ParseReportSpec where

import           BasicPrelude
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.KeyMap             as KM
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BLC
import           Data.Time                      ( fromGregorian )
import           Test.Hspec

import           HtmlUtil                       ( makeHtml )
import           Models.Chapter                as Chapter
import           Models.NRS                     ( NRS(..) )
import           Models.ParseFailure            ( atChapter
                                                , atSection
                                                , parseFailure
                                                )
import           Models.Section                 ( Section(..)
                                                , toSectionBody
                                                , toSectionName
                                                , toSectionNumber
                                                )
import           Models.Title                  as Title
import           Models.Tree                    ( Tree(..) )
import           ParseReport
import           Util                           ( (|>) )
import           Year                           ( toYear )


spec :: SpecWith ()
spec = parallel $ do
  describe "summarize" $ do
    it "counts chapters and sections from the tree and failures" $ do
      let nrs   = sampleNrs
          fails =
            [ parseFailure "NRS-002.html" "Chapter file not found: NRS-002.html"
                |> atChapter "2"
            , parseFailure "NRS-001.html" "Section body is blank"
                |> atChapter "1"
                |> atSection "1.020"
            ]
          s     = summarize nrs fails
      chaptersAttempted s `shouldBe` 3
      chaptersFailed s `shouldBe` 1
      chaptersSucceeded s `shouldBe` 2
      sectionsSucceeded s `shouldBe` 1
      sectionsFailed s `shouldBe` 1
      sectionsAttempted s `shouldBe` 2
      sectionFailureRate s `shouldBe` 0.5
      chapterFailureRate s `shouldBe` (1 / 3)

  describe "renderParseResult" $ do
    it "puts pretty NRS on stdout bytes and JSONL plus summary on stderr" $ do
      let fails =
            [ parseFailure "NRS-002.html" "Chapter file not found: NRS-002.html"
                |> atChapter "2"
            , parseFailure "NRS-001.html" "Section body is blank"
                |> atChapter "1"
                |> atSection "1.020"
            ]
          (out, err) = renderParseResult sampleNrs fails
          errLines   = BLC.lines err
      BL.null out `shouldBe` False
      case Aeson.decode out :: Maybe Aeson.Value of
        Nothing -> expectationFailure "stdout was not JSON"
        Just _  -> pure ()
      length errLines `shouldBe` 3
      case Aeson.decode (last errLines) :: Maybe (KM.KeyMap Aeson.Value) of
        Nothing  -> expectationFailure "summary line was not JSON"
        Just obj -> do
          obj KM.!? "kind" `shouldBe` Just (Aeson.String "summary")
          obj KM.!? "chaptersFailed" `shouldBe` Just (Aeson.Number 1)
          obj KM.!? "sectionsFailed" `shouldBe` Just (Aeson.Number 1)
      case Aeson.decode (head errLines) :: Maybe (KM.KeyMap Aeson.Value) of
        Nothing -> expectationFailure "first stderr line was not JSON"
        Just obj ->
          obj KM.!? "chapterNumber" `shouldBe` Just (Aeson.String "2")


sampleNrs :: NRS
sampleNrs = NRS
  { statuteTree  = Tree
      { chapter0 = emptyShell "0" "Preliminary"
      , titles   =
          [ Title
              { Title.name     = "Title 1"
              , Title.number   = 1
              , Title.chapters =
                  [ Chapter
                      { Chapter.name    = "Good"
                      , Chapter.number  = "1"
                      , Chapter.url     = "https://example.test/1.html"
                      , Chapter.content = SimpleChapterContent [goodSection]
                      }
                  , emptyShell "2" "Missing"
                  ]
              }
          ]
      }
  , nominalDate  = toYear 2018
  , dateAccessed = fromGregorian 2020 1 1
  }


emptyShell :: Text -> Text -> Chapter
emptyShell number' name' = Chapter
  { Chapter.name    = name'
  , Chapter.number  = number'
  , Chapter.url     = "https://example.test/" ++ number' ++ ".html"
  , Chapter.content = ComplexChapterContent []
  }


goodSection :: Section
goodSection =
  case ( toSectionName "Name" "ctx"
       , toSectionNumber "1.010" "ctx"
       , toSectionBody (makeHtml "<p>body</p>") "ctx"
       ) of
    (Right n, Right num, Right b) ->
      Section { name = n, number = num, body = b }
    _ -> error "fixture section constructors failed"


main :: IO ()
main = hspec spec
