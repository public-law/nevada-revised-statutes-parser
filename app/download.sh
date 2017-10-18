#!/usr/bin/env bash
# Download the NRS HTML files into /tmp/www.leg.state.nv.us

cd /tmp
rm -rf www.leg.state.nv.us
wget --recursive --no-parent https://www.leg.state.nv.us/NRS/
