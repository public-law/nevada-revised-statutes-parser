Parser for the Nevada Revised Statutes 
======================================

**Input:** The Nevada laws (i.e., the Nevada Revised Statutes) [website](https://www.leg.state.nv.us/NRS/).

**Output:** Semantic JSON:

```json
{
    "chapterName": "Protection of Children from Abuse and Neglect",
    "chapterNumber": "432B",
    "chapterUrl": "https://www.leg.state.nv.us/nrs/NRS-432B.html",
    "subChapters": [
        {
            "subChapterName": "General Provisions",
            "subChapterChildren": {
                "tag": "SubChapterSections",
                "contents": [
                    {
                        "sectionNumber": "432B.010",
                        "sectionName": "Definitions.",
                        "sectionBody": "<p class=SectBody><span class=\"Section\">432B.010</span>..."
                    },
                    {
                        "sectionNumber": "432B.020",
                        "sectionName": "“Abuse or neglect of a child” defined.",
                        "sectionBody": "<p class=SectBody><span class=\"Section\">432B.020</span>..."
                    },

                    # ...
                    
                ]
            }
        }
    ]
}
```

The [Nevada Laws](http://www.nevadalaws.org) web app will read in and display the content.
