#!/bin/bash

set ex

nix-build deploy.nix

cd result/
zip -r /output/bailiwick-static.zip static 


