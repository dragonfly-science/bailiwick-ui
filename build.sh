#!/bin/bash

set ex

nix-build deploy.nix

cd result/
zip -qr /output/bailiwick-static.zip static 

aws s3 cp --recursive static/ s3://gorbachev.io/dragonfly-science/bailiwick-ui/

