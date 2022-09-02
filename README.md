Parser for the Nevada Revised Statutes
======================================

**Input:** The [Nevada Revised Statutes website](https://www.leg.state.nv.us/NRS/),
mirrored with the included script into `/tmp`.

**Output:** Well-formed JSON which implements the laws' structure as the legislature created it:

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

Explore the Data on Kaggle
--------------------------

* https://www.kaggle.com/public-law/nevadanrs

Opinionated Resources for Haskell Best Practices
------------------------------------------------

* [An Opinionated Guide to Haskell in 2018](https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/)
* [Haskell Porting Notes](https://github.com/srid/slownews/blob/master/notes/haskell-port.md)
* [Haskell Package Checklist](http://taylor.fausak.me/haskell-package-checklist/)
* [Haskell Data Types in 5 Steps](https://mmhaskell.com/blog/2017/12/24/haskell-data-types-in-5-steps)

On Exception Handling
---------------------

* [Monadic Either](https://www.reddit.com/r/haskellquestions/comments/a6xzdj/library_for_easy_newtype_generation_eg_data/ec5b3rz/)
* [Pure Functional Validation](https://medium.com/blacklane-engineering/pure-functional-validation-64a7885d22ac)
* [Eight Ways to Report Errors in Haskell](http://www.randomhacks.net/2007/03/10/haskell-8-ways-to-report-errors/)
