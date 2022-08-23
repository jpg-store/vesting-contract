#!/usr/bin/env bash

set -eux
mkdir -p scripts/temp/
mkdir -p ~/$BLOCKCHAIN_PREFIX
./scripts/wallets/make-wallet-and-pkh.sh benefactor
./scripts/wallets/make-wallet-and-pkh.sh beneficiary
./scripts/wallets/make-wallet-and-pkh.sh beneficiary1
./scripts/wallets/make-wallet-and-pkh.sh beneficiary2
./scripts/wallets/make-wallet-and-pkh.sh beneficiary3
