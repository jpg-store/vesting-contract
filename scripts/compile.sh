set -eu
thisDir=$(dirname "$0")
mainDir=$thisDir/..
tempDir=$mainDir/temp

(
	cd $mainDir
	cabal run vesting-sc -- write --output=assets/local-testnet/vesting.plutus
	cabal run vesting-sc -- write --output=assets/testnet/vesting.plutus
	cabal run vesting-sc -- write --output=assets/mainnet/vesting.plutus
)

$thisDir/hash-plutus.sh
