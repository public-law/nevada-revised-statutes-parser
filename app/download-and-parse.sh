#!/usr/bin/env bash

# Download the NRS HTML files into /tmp/www.leg.state.nv.us
cd /tmp
rm -rf www.leg.state.nv.us
wget --recursive --no-parent --no-verbose https://www.leg.state.nv.us/NRS/

# Parse and produce JSON
cd -
.stack-work/install/x86_64-osx/lts-12.13/8.4.3/bin/nrs-parser-exe
