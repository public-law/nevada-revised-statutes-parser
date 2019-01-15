#!/usr/bin/env bash

# Download the NRS HTML files into /tmp/www.leg.state.nv.us
cd /tmp
wget --recursive --no-parent --no-verbose https://www.leg.state.nv.us/NRS/

# Parse and produce JSON
cd -
nrs-parser-exe
