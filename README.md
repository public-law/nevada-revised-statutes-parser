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

Build and test
--------------

Requires [Stack](https://docs.haskellstack.org/). The project pins Stackage
`lts-24.65` (GHC 9.10.3) via `stack.yaml` / `stack.yaml.lock`.

```bash
stack setup
stack test
```

To parse a local mirror of the NRS site:

```bash
./scripts/download.sh
stack exec nrs-parser-exe 2> errata.jsonl > nevada-nrs.json
```

- **stdout:** pretty-printed NRS JSON (same shape as before; safe for existing redirects).
- **stderr:** one JSON Line per `ParseFailure`, then a final summary object
  (`"kind":"summary"`) with chapter and section attempted / succeeded / failed
  counts and failure rates. Use `tail -1 errata.jsonl` for the summary alone.

The process exits 0 when an NRS document was produced, even if some chapters or
sections failed. `Year.toYear` and `FileUtil` path newtypes remain hard errors
for programmer invariants.

Internally, recoverable parse failures accumulate in `ParseResult` (`Writer
[ParseFailure]`); structural parsers use `ParseResultT (Either String)` so a
structural failure still discards that parser's partial result. `tell` records
failures, `censor` adds enclosing title/chapter locations, and `mapM` preserves
encounter order. `parseNRS` runs the writer and retains the `(NRS, [ParseFailure])`
interface used by the executable.

Explore the Data on Kaggle
--------------------------

* https://www.kaggle.com/public-law/nevadanrs
