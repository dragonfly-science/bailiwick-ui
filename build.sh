#!/bin/bash

set -ex

## Make the d3, non-reflex javascript
nix-shell javascript.nix -A shell --command 'ln -sf $NODE_PATH; npm run build'

## make the javascript executible
nix-build deploy.nix

## Make the data assets
nix-shell db/default.nix --run 'make -BC db'
rm result/static/db
mkdir result/static/db/
cp -r db/dev result/static/db/

## make the validate executible TODO make this work
nix-env -f . -i -A ghc.bailiwick
bailiwick-validate db/dev

cd result/
zip -qr /output/bailiwick-static.zip static

aws s3 cp --recursive static/ s3://gorbachev.io/dragonfly-science/bailiwick-ui/

echo Copied to s3://gorbachev.io/dragonfly-science/bailiwick-ui/
