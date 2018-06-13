#!/bin/sh

set ex

nix-build deploy.nix

cd result/
zip -r /output/bailiwick-static.zip static 

aws s3 cp --quiet --recursive static/ s3://gorbachev.io/dragonfly-science/bailiwick-ui/

