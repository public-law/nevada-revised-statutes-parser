nv-statutes
===========

Input: The Nevada "laws" (i.e., the Nevada Revised Statutes) directly from [the website](https://www.leg.state.nv.us/NRS/).

Output: JSON with as much semantic information as possible, similar to my [Oregon parser](https://github.com/dogweather/analyze-oregon-law-haskell):

```json
{
    "summary": "Relating to speed limits on highways that traverse state lines; creating new provisions; amending ORS 811.111; and declaring an emergency."
    "bill": {
        "billNumber": 4047,
        "billType": "HB"
    }, 
    "effectiveDate": "2016-03-01",
    "year": 2016,
    "affectedSections": {
        "repealed": [],
        "amended": [
            "811.111"
        ]
    }
}
```
