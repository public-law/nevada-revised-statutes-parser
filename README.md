Parser for the Nevada Revised Statutes 
======================================

[![Build Status](https://travis-ci.org/public-law/nevada-revised-statutes-parser.svg?branch=master)](https://travis-ci.org/public-law/nevada-revised-statutes-parser)

**Input:** The [Nevada Revised Statutes website](https://www.leg.state.nv.us/NRS/), downloaded with `wget` into `/tmp`.

**Output:** Semantic JSON:

```json
{
    "nominalDate": 2018,
    "dateAccessed": "2018-01-28",
    "statuteTree": {
        "titles": [
            {
                "chapters": [
                    {
                        "url": "https://www.leg.state.nv.us/nrs/NRS-1.html",
                        "name": "Judicial Department Generally",
                        "number": "1",
                        "subChapters": [
                            {
```

etc.

----

Archived Code Fragments
-----------------------

```haskell
-- Accepts encodings such as "LATIN1".
-- Not currently in use.
readFileAsUtf8 :: FilePath -> String -> IO Text
readFileAsUtf8 pathname sourceEncoding = do
    let stdin' = ""
    stdout' <- readProcess "iconv" ["-f", sourceEncoding, "-t", "utf-8", pathname] stdin'
    return $ T.pack stdout'
```

----

Resources
---------

* https://github.com/srid/slownews/blob/master/notes/haskell-port.md
* http://taylor.fausak.me/haskell-package-checklist/
