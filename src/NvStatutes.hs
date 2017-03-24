{-# LANGUAGE OverloadedStrings #-}

module NvStatutes where

import           BasicPrelude
import           Control.Category.Unicode
import           Data.Function            ((&))
import           Data.List.Split          (chunksOf, split, whenElt)
import           Data.Text                (splitOn, strip)
import           Models
import           Text.HTML.TagSoup        (Tag, fromAttrib, innerText,
                                           parseTags, partitions, (~==))
import Text.Parser.Token
import Text.Parser.Token.Style
import Text.Parser.Combinators
import Data.Attoparsec.Text (parseOnly)


titles ∷ Text → [Title]
titles indexHtml =
  let rows   = contentRows indexHtml
      tuples = rowTuples rows
  in fmap newTitle tuples


contentRows ∷ Text → [[Tag Text]]
contentRows indexHtml =
  let tags       = parseTags indexHtml
      table      = head $ partitions (~== s "<table class=MsoNormalTable") tags
  in partitions (~== s "<tr>") table


newTitle ∷ [[[Tag Text]]] -> Title
newTitle tuple =
  let titleRow       = head (head tuple)
      chapterRows    = head $ tail tuple
      parsedChapters = fmap newChapter chapterRows
      rawTitle       = innerText $ head $ partitions (~== s "<b>") titleRow
      name           = nameFromRawTitle rawTitle
      number         = numberFromRawTitle rawTitle
  in Title { titleName = name, titleNumber = number, chapters = parsedChapters }


newChapter ∷ [Tag Text] → Chapter
newChapter row =
  Chapter {
    chapterName   = name,
    chapterNumber = number,
    chapterUrl    = url,
    subChapters   = []
  }
  where columns = partitions (~== s "<td>") row
        number  = head columns & innerText & strip & words & last
        name    = last columns & innerText & strip
        url     = partitions (~== s "<a>") row & head & head & fromAttrib "href"


-- Input:  "TITLE\n  1 \8212 STATE JUDICIAL DEPARTMENT\n  \n \n "
-- Output: "STATE JUDICIAL DEPARTMENT"
nameFromRawTitle ∷ Text → Text
nameFromRawTitle text =
  splitOn "—" text
    & tail
    & head
    & strip


-- Input:  "TITLE\n  1 — STATE JUDICIAL DEPARTMENT\n  \n \n "
-- Output: 1
numberFromRawTitle ∷ Text → Integer
numberFromRawTitle =
  numberTextFromRawTitle


-- Input:  "TITLE\n  1 — STATE JUDICIAL DEPARTMENT\n  \n \n "
-- Output: "1"
numberTextFromRawTitle ∷ Text → Integer
numberTextFromRawTitle input =
  let f = parseOnly p input
      p = textSymbol "TITLE" *> integer
  in case f of
    Left e -> error e
    Right b -> b
    --
    -- splitOn "—"
    --   ⋙ head
    --   ⋙ strip
    --   ⋙ splitOn "\n"
    --   ⋙ tail
    --   ⋙ head



rowTuples ∷ [[Tag Text]] → [[[[Tag Text]]]]
rowTuples rows =
  split (whenElt isTitleRow) rows
    & tail
    & chunksOf 2


isTitleRow ∷ [Tag Text] → Bool
isTitleRow r =
  length (partitions (~== s "<td>") r) == 1


-- Lower-ceremony way to declare a string
s ∷ String → String
s = id
