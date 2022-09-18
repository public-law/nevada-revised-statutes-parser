#!/usr/bin/env bash

set -x
set -e

# Download the NRS HTML files into /tmp/www.leg.state.nv.us
cd /tmp
wget --recursive --no-parent --no-verbose https://www.leg.state.nv.us/NRS/

# Parse and produce JSON
cd - > /dev/null  # "cd -" echoes the new dir to stdout.
stack exec nrs-parser-exe > nevada-nrs.json

# Upload result to S3
s3cmd put nevada-nrs.json s3://publaw-scraping-results
