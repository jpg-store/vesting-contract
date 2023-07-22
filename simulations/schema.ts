import { Data } from "https://deno.land/x/lucid@0.10.4/mod.ts";

const VestingSchema = Data.Object({
  beneficiaries: Data.Array(Data.Bytes()),
  portions: Data.Array(
    Data.Object({
      deadline: Data.Integer(),
      amount: Data.Map(Data.Bytes(), Data.Map(Data.Bytes(), Data.Integer())),
    })
  ),
});

const VestingActionSchema = Data.Object({
  newKeys: Data.Array(Data.Bytes()),
});

type Vesting = Data.Static<typeof VestingSchema>;
type VestingAction = Data.Static<typeof VestingActionSchema>;

export type PlutusValue = Map<string, Map<string, bigint>>;

export const Vesting = VestingSchema as unknown as Vesting;
export const VestingAction = VestingActionSchema as unknown as VestingAction;
