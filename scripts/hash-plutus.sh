#!/usr/bin/env bash

set -eu

thisDir=$(dirname "$0")
mainDir=$thisDir/..
assetDir=$mainDir/assets

cardano-cli address build \
	--payment-script-file $assetDir/mainnet/vesting.plutus \
	--mainnet \
	--out-file $assetDir/mainnet/vesting.addr

cardano-cli address build \
	--payment-script-file $assetDir/testnet/vesting.plutus \
	--testnet-magic 1097911063 \
	--out-file $assetDir/testnet/vesting.addr

cardano-cli address build \
	--payment-script-file $assetDir/local-testnet/vesting.plutus \
	--testnet-magic 42 \
	--out-file $assetDir/local-testnet/vesting.addr
