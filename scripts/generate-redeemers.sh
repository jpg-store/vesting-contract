set -eu
thisDir=$(dirname "$0")
tempDir=$thisDir/../temp

DATUM_PREFIX=${DATUM_PREFIX:-0}

mkdir -p $tempDir/$BLOCKCHAIN_PREFIX/redeemers/$DATUM_PREFIX

beneficiary=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/beneficiary-pkh.txt)
beneficiary1=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/beneficiary1-pkh.txt)
beneficiary2=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/beneficiary2-pkh.txt)
beneficiary3=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/beneficiary3-pkh.txt)

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/redeemers/$DATUM_PREFIX/disburse.json
{
  "constructor": 0,
  "fields": [
    {
      "list": [
        {
          "bytes": "$beneficiary1"
        },
        {
          "bytes": "$beneficiary2"
        },
        {
          "bytes": "$beneficiary3"
        }
      ]
    }
  ]
}

EOF
