#!/usr/bin/env bash

set -x
set -e

# Download the NRS HTML files into /tmp/www.leg.state.nv.us
cd /tmp
wget --recursive --no-parent --no-verbose https://www.leg.state.nv.us/NRS/

# Parse and produce JSON. Prefer an installed binary (Docker), else Stack.
cd - > /dev/null
if command -v nrs-parser-exe >/dev/null 2>&1; then
  nrs-parser-exe > nevada-nrs.json
else
  stack exec nrs-parser-exe > nevada-nrs.json
fi

# Upload result to S3 when s3cmd is available
if command -v s3cmd >/dev/null 2>&1; then
  s3cmd put nevada-nrs.json s3://publaw-scraping-results
fi
