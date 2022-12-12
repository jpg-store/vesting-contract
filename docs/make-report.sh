#!/usr/bin/env sh
nix run github:mlabs-haskell/GHAppy/main -- \
	-a $(cat ./.token.txt) \
	-o _out \
   -f $(date --iso)-JPG-Vesting-Contract-Audit-Report \
	-r jpg-store/vesting-contract \
	-u cstml \
	-i ./report.yaml
