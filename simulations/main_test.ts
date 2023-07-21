import { FakeTime } from "https://deno.land/std@0.195.0/testing/time.ts";
const time = new FakeTime(0);

import { Emulator, Lucid } from "https://deno.land/x/lucid@0.10.4/src/mod.ts";
import { mapAsync } from "./utils.ts";
import { Data, Script } from "https://deno.land/x/lucid@0.10.4/mod.ts";
import { Vesting, VestingAction } from "./schema.ts";

const JPG_ASSET_NAME = "ffff";
const JPG_CURRENCY_SYMBOL =
  "0fffffffffffffffffffffffffffffffffffffffffffffffffffffff";
const JPG_TOTAL_SUPPLY = 1_000_000_000_000n; // 1 billion

const beneficiariesNames = ["Martin", "Travis", "Edward"];

let lucid = await Lucid.new();

const beneficiaries = await mapAsync(beneficiariesNames, async (name) => {
  const privateKey = lucid.utils.generatePrivateKey();
  return {
    name,
    privateKey,
    address: await lucid
      .selectWalletFromPrivateKey(privateKey)
      .wallet.address(),
  };
});

const emulator = new Emulator(
  beneficiaries.map((beneficiary, idx) => ({
    address: beneficiary.address,
    assets: {
      [JPG_CURRENCY_SYMBOL + JPG_ASSET_NAME]: idx === 0 ? JPG_TOTAL_SUPPLY : 0n,
      lovelace: 1_000_000_000_000n,
    }, // 1M ADA each
  }))
);

lucid = await Lucid.new(emulator);

lucid.selectWalletFromPrivateKey(beneficiaries[0].privateKey);

const me = await lucid.wallet.address();

const validator = await getValidator();
const vestingAddr = lucid.utils.validatorToAddress(validator);

Deno.test("Normally vesting $JPG", async () => {
  await vest({
    beneficiaries: beneficiaries.map(
      (beneficiary) =>
        lucid.utils.getAddressDetails(beneficiary.address).paymentCredential
          ?.hash!
    ),
    portions: [
      {
        amount: $jpg(JPG_TOTAL_SUPPLY / 2n),
        deadline: 100n,
      },
      {
        amount: $jpg(JPG_TOTAL_SUPPLY / 4n),
        deadline: 200n,
      },
      {
        amount: $jpg(JPG_TOTAL_SUPPLY / 4n),
        deadline: 300n,
      },
    ],
  });

  emulator.awaitBlock(1);

  await tryUnvest(JPG_TOTAL_SUPPLY / 2n, JPG_TOTAL_SUPPLY);
});

function $jpg(amt: bigint) {
  const inner = new Map();
  inner.set(JPG_ASSET_NAME, amt);
  const outer = new Map();
  outer.set(JPG_CURRENCY_SYMBOL, inner);

  return outer;
}

async function vest(vesting: typeof Vesting) {
  console.log("Vesting money...");
  const datum = Data.to(vesting, Vesting);

  const txComplete = await lucid
    .newTx()
    .payToContract(vestingAddr, datum, {
      [JPG_CURRENCY_SYMBOL + JPG_ASSET_NAME]: JPG_TOTAL_SUPPLY,
    })
    .complete();

  const signed = await txComplete.sign().complete();
  await signed.submit();
  console.log("Money has been vested!");
}

async function tryUnvest(jpgAmount: bigint, jpgRemaining: bigint) {
  try {
    console.log("Trying to unvest...");

    const utxosAtVesting = await lucid.utxosAt(vestingAddr);
    const vestingUtxo = utxosAtVesting[0];
    const prevDatum = await lucid.datumOf(vestingUtxo);

    const redeemer = Data.to(
      {
        newKeys: beneficiaries.map(
          (b) =>
            lucid.utils.getAddressDetails(b.address).paymentCredential?.hash!
        ),
      },
      VestingAction
    );

    time.tick(2000);

    const txComplete = await lucid
      .newTx()
      .collectFrom(utxosAtVesting, redeemer)
      .attachSpendingValidator(validator)
      .addSigner(me)
      .validFrom(time.now)
      .addSigner(beneficiaries[1].address) // 2/3
      .payToContract(vestingAddr, prevDatum.toString(), {
        [JPG_CURRENCY_SYMBOL + JPG_ASSET_NAME]: jpgRemaining - jpgAmount,
      })
      .payToAddress(me, {
        [JPG_CURRENCY_SYMBOL + JPG_ASSET_NAME]: jpgAmount,
      })
      .complete();

    const signed = await txComplete
      .sign()
      .signWithPrivateKey(beneficiaries[1].privateKey)
      .complete();

    await signed.submit();
    console.log("Unvesting successful!");
  } catch (e) {
    console.error("Unvesting unsuccessful: ", e);
  }
}

async function getValidator(): Promise<Script> {
  const validator = await Deno.readTextFile("../assets/testnet/vesting.plutus");
  const oldSchemaValidator = JSON.parse(validator);

  return {
    script: oldSchemaValidator.cborHex,
    type: "PlutusV2",
  };
}
