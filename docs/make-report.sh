#!/usr/bin/env sh
rm -r ./_out
nix run github:mlabs-haskell/GHAppy/main -- \
	-a $(cat ./.token.txt) \
	-o _out \
   -f $(date --iso)-JPG-Vesting-Contract-Audit-Report \
	-r jpg-store/vesting-contract \
	-u cstml \
	-i ./report.yaml

find ./_out | xargs sha256sum > _out/checksum.txt
