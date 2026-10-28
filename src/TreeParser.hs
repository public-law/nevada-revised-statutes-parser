module TreeParser
  ( parseTree
  )
where

import           BasicPrelude
import qualified Data.HashMap.Lazy             as HM
import qualified Data.Text                     as T
import           Text.InterpolatedString.Perl6  ( qq )

import           ChapterFile                    ( ChapterMap
                                                , parseChapter
                                                )
import           Config
import           HtmlUtil                       ( Html )
import           IndexFile                      ( parseTitlesAndChapters )
import           Models.Chapter
import           Models.ParseFailure            ( ParseFailure
                                                , atChapter
                                                , parseFailure
                                                )
import           Models.Tree
import           Util                           ( (|>) )


parseTree :: Html -> ChapterMap -> (Tree, [ParseFailure])
parseTree indexFile chapterMap =
  let (chapter0', zeroFails) = parseChapterZero chapterMap
      (titles', titleFails) =
        parseTitlesAndChapters indexFile (allButChapterZero chapterMap)
  in  ( Tree { chapter0 = chapter0', titles = titles' }
      , zeroFails ++ titleFails
      )


parseChapterZero :: ChapterMap -> (Chapter, [ParseFailure])
parseChapterZero chapterMap =
  let path        = chapterZeroPathname
      placeholder = emptyChapterZero
  in  case HM.lookup path chapterMap of
        Just html -> case parseChapter html of
          Left message ->
            ( placeholder
            , [ parseFailure "NRS-000.html" (T.pack message)
                  |> atChapter "0"
              ]
            )
          Right (aChapter, fails) -> (aChapter, fails)
        Nothing ->
          ( placeholder
          , [ parseFailure
                "NRS-000.html"
                [qq|Chap. Zero not found in {show $ take 1 $ HM.keys chapterMap}|]
                |> atChapter "0"
            ]
          )


emptyChapterZero :: Chapter
emptyChapterZero = Chapter
  { name    = "Preliminary Chapter – General Provisions"
  , number  = "0"
  , url     = chapterUrlPrefix ++ "0.html"
  , content = SimpleChapterContent []
  }


allButChapterZero :: ChapterMap -> ChapterMap
allButChapterZero = HM.delete chapterZeroPathname
