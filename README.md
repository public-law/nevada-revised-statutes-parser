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
stack exec nrs-parser-exe > nevada-nrs.json
```

Explore the Data on Kaggle
--------------------------

* https://www.kaggle.com/public-law/nevadanrs
