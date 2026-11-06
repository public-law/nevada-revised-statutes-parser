module TreeParser
  ( parseTree
  )
where

import           BasicPrelude
import           Control.Monad.Writer.Strict    ( runWriterT
                                                , tell
                                                , writer
                                                )
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
import           Models.ParseFailure            ( ParseResult
                                                , atChapter
                                                , parseFailure
                                                )
import           Models.Tree
import           Util                           ( (|>) )


parseTree :: Html -> ChapterMap -> ParseResult Tree
parseTree indexFile chapterMap = do
  chapter0' <- parseChapterZero chapterMap
  titles' <- parseTitlesAndChapters indexFile (allButChapterZero chapterMap)
  pure Tree { chapter0 = chapter0', titles = titles' }


parseChapterZero :: ChapterMap -> ParseResult Chapter
parseChapterZero chapterMap =
  let fallback msg = do
        tell [parseFailure "NRS-000.html" msg |> atChapter "0"]
        pure emptyChapterZero
  in  case HM.lookup chapterZeroPathname chapterMap of
        Just html -> case runWriterT (parseChapter html) of
          Left message -> fallback (T.pack message)
          Right result -> writer result
        Nothing -> fallback
          [qq|Chap. Zero not found in {show $ take 1 $ HM.keys chapterMap}|]


emptyChapterZero :: Chapter
emptyChapterZero = Chapter
  { name    = "Preliminary Chapter – General Provisions"
  , number  = "0"
  , url     = chapterUrlPrefix ++ "0.html"
  , content = SimpleChapterContent []
  }


allButChapterZero :: ChapterMap -> ChapterMap
allButChapterZero = HM.delete chapterZeroPathname
