---
title: "**JPG Vesting Contract**"
subtitle: "Audit Report"
author: "MLabs Audit Team"
date: \today
titlegraphic: "./linked-files/images/MLabs-logo.jpg"
logo: "./linked-files/images/MLabs-logo.jpg"
fontsize: 10
colorlinks: true
graphics: true
title-on: true
block-headings: true
numbersections: true

---
# Disclaimer

**This audit report is presented without warranty or guarantee of any type. Neither MLabs nor its auditors can assume any liability whatsoever for the use, deployment or operations of the audited code.** This report lists the most salient concerns that have become apparent to MLabs’ auditors after an inspection of the project's codebase and documentation, given the time available for the audit. Corrections may arise, including the revision of incorrectly reported issues. Therefore, MLabs advises against making any business or other decisions based on the contents of this report.

An audit does not guarantee security. Reasoning about security requires careful considerations about the capabilities of the assumed adversaries. These assumptions and the time bounds of the audit can impose realistic constraints on the exhaustiveness of the audit process. Furthermore, the audit process involves, amongst others, manual inspection and work which is subject to human error.

**MLabs does not recommend for or against the use of any work or supplier mentioned in this report.** This report focuses on the technical implementation provided by the project's contractors and subcontractors, based on the information they provided, and is not meant to assess the concept, mathematical validity, or business validity of their product. This report does not assess the implementation regarding financial viability nor suitability for any purpose. *MLabs does not accept responsibility for any loss or damage howsoever arising which may be suffered as result of using the report nor does it guarantee any particular outcome in respect of using the code on the smart contract.*


\newpage

# Background

## Scope

During the audit, MLabs Audit Team (from now on referred to as MLabs) have used the provided files for the following scope: 
 
- **A. Audit the onchain contract**

- [X] A.1. Integrate testing frameworks (Plutip and PSM) to allow reproducible testing of hypotheses. 

- [X] A.2. Write tests to prove the well functioning of the on-chain components.

- **B. Audit of the offchain components**

- [ ] B.1. Test the well functioning of the  provided shell scripts.

- [X] B.2. Audit the shell scripts for any malicious or not intended behaviour.

Please note that point **B.1.** is not marked as finished due to it being partially explored. For more information please refer to [_Vesting Contract Offchain Hypotheses_](https://github.com/jpg-store/vesting-contract/issues/10). 

## Methodology

### Timeline

In response to the above scope, the Audit process took three (one week) sprints and it can be summarised to the following actions:

1. Review and test the onchain/offchain components against the [MLabs Vulnerability types](https://github.com/mlabs-haskell/audit-report-template/blob/master/linked-files/vulnerability-types.md).   

2. Write test scenarios for the implementations, and run some of them against a Cardano node (via Plutip).

3. Find optimisations, code quality improvements, or recommendations. 

4. Capture the findings in an Audit Report. 

### Information

MLabs analysed the validators and minting scripts from the github.com/jpg-store/[vesting-contract](https://github.com/jpg-store/vesting-contract) repository starting at commit `703566b`.

### Audited Files Checksums

The following checksums are those of files captured by commit `703566b`, and were generated using the following sha256 binary:

```
$ sha256sum --version
sha256sum (GNU coreutils) 9.0
```

The checksums are:

```
34ed...a6a6  app/Main.hs
6ef0...aa6c  src/Canonical/Shared.hs
ee50...1886  src/Canonical/DebugUtilities.h
add6...b689  src/Canonical/Vesting.hs
```

### Audit Report

The audit report is an aggregation of issue, tickets and pull-requests created in the [jpg-store/vesting-contract](https://github.com/jpg-store/vesting-contract) repository. 

### Metrics

#### CVSS

To leverage a standardised metric for the severity of the open standard [Common Vulnerability Scoring System][cvss], together with the [NVD Calculator][nvd-1]. The metrics from the mentioned tools were included with each vulnerability. MLabs recognises that some of the parameters are not conclusive for the protocol - but considers that leveraging such a standard is still valuable to offer a more unbiased severity metric for the found vulnerabilities.

#### Severity Levels

The aforementioned CVSS calculations were then benchmarked using the [CVSS-Scale][cvss-scale] metric, receiving a grade spanning from `Low` to `Critical`. This additional metric allows for an easier, human understandable grading, whilst leveraging the CVSS standardised format.

<!-- Refs -->
[cvss-scale]: https://www.first.org/cvss/v3.1/specification-document#Qualitative-Severity-Rating-Scale "CVSS-Scale"
[cvss]: https://www.first.org/cvss/ "Common Vulnerability Scoring System"
[nvd-1]: https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator "NVD Calculator"


\newpage

# Findings

## Summary

The Audit revealed the following two `Medium Severity` vulnerabilities:

- [_Input Datum - Unbounded List_](https://github.com/jpg-store/vesting-contract/pull/12) of type `unbounded-protocol-datum`,

-  [_Early Unlocking of Vesting Value_](https://github.com/jpg-store/vesting-contract/pull/23) of type `incorrect-logic`. 

Furthermore, the audit puts forward the following optimisations / recommendations:

- [_Coding Standards - Linting_](https://github.com/jpg-store/vesting-contract/pull/4)

- [_Coding Standards - Formatters_](https://github.com/jpg-store/vesting-contract/pull/5)

- [_Optimisation - Calculating the Total Unvested Value_](https://github.com/jpg-store/vesting-contract/pull/14)

Please note that as described in [_Vesting Contract Offchain Hypotheses_](https://github.com/jpg-store/vesting-contract/issues/10), only a partial audit of the offchain code was undertaken - not revealing any vulnerabilities. 


\newpage

## Vulnerabilities

### Input Datum - Unbounded List

| Severity | CVSS | Vulnerability type |
| --- | --- | --- |
| Medium | [6.8](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator?vector=AV:N/AC:L/PR:N/UI:N/S:C/C:N/I:L/A:L/E:F/RL:U/RC:R&version=3.1) | [unbounded-protocol-datum](https://www.notion.so/Unbounded-Protocol-datum-46ad4efb9bd44dd983159644d4a2ce76) |

#### Description

The Input datum of the vesting contract is the product of two unbounded lists of beneficiaries and schedule. Due to the unbounded nature of the datum, it is possible to make arbitrary long vesting schedules for number of arbitrary beneficiaries, however doing so may result in permanently locking the native tokens and ADA in the vesting contract. This is because when contract is executed while spending it, it exceeds the resource limit which is imposed on the transaction.

#### How To Reproduce

Checkout branch [audit/unbounded-datum](https://github.com/jpg-store/vesting-contract/tree/audit/unbounded-datum) and run:

```bash
$ cabal run vesting-tests
```

#### Expected behaviour

The values locked in the contract should not be locked permanently.

#### Error

While spending the contract it results in the following error:

```
ContractExecutionError "WalletContractError (OtherError \"ScriptFailure 
(ScriptErrorEvaluationFailed 
(CekError An error has occurred:  User error:
The machine terminated part way through evaluation due to overspending the budget.\
The budget when the machine terminated was:
({ cpu: 7057872449
| mem: -852\
})
Negative numbers indicate the overspent budget; note that this only indicates the budget that was 
needed for the next step, not to run the program to completion.) [])\")"   
```

#### Provided Proof

The following plutip test demonstrate this vulnerability: [unboundedDatum](https://github.com/jpg-store/vesting-contract/blob/audit/unbounded-datum/tests/Spec/Vesting.hs#L121).


### Early Unlocking of Vesting Value

| Severity | CVSS | Vulnerability type |
| --- | --- | --- |
| Medium | [5.5](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator?vector=AV:N/AC:L/PR:N/UI:N/S:C/C:N/I:L/A:N/E:F/RL:U/RC:R&version=3.1) | [incorrect-logic](https://www.notion.so/Incorrect-logic-5d1945367d9446fba8cd825744d1de3e) |

#### Description

The `Input` datum of the vesting contract contains a list of `Portion`, which is the product of two types `Value` and `POSIXTime`. As part of Portion, it's possible to include negative values (e.g. -1000 Ada), which allows an early unlocking of other `Portion`(s) with the same `AssetClass` as the `Portion` (i.e. unlocking the locked value before the vesting deadline)  with negative values (e.g A portion with 1000 Ada).

#### How To Reproduce

Checkout branch [audit/negative-value-in-input](https://github.com/jpg-store/vesting-contract/tree/audit/negative-value-in-input) and run:

```bash
$ cabal run vesting-tests
```

#### Expected behaviour

Early unlocking of the vested value should not be possible and it's not quite right to be able to include negative values in the `Portion`.

#### Provided proof

The following plutip test demonstrate this vulnerability: [negativeValueDatum](https://github.com/jpg-store/vesting-contract/blob/audit/negative-value-in-input/tests/Spec/Vesting.hs#L159).


\newpage

## Recommendations

### Coding Standards - Linting

| Severity | CVSS | Vulnerability type |
| --- | --- | --- |
| None | [0.0](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator?vector=AV:N/AC:L/PR:N/UI:N/S:C/C:N/I:N/A:N/E:U/RL:X/RC:X/CR:H/IR:H/AR:M/MAV:X/MAC:X/MPR:X/MUI:X/MS:X/MC:N/MI:N/MA:N&version=3.1) | `poor-code-standards` |

#### Description

Use shellcheck to perform static analysis of the `shell` scripts used in the offchain of the protocol to find optimisations, and underlying bugs. Use `hlint` to lint the Haskell code. 

#### Vulnerabilities

1. Please refer to [./docs/audit/shell-check-report.txt](https://github.com/jpg-store/vesting-contract/blob/audit/shellcheck/docs/audit/shell-check-report.txt) for a detailed list of all the bugs, vulnerabilities, and optimisations. Note that the `shellcheck` analysis is based on the assumption that the scripts are intended to be interpreted via `/bin/bash`.
 
3. Please refer to [./docs/audit/hlint-check-report.txt](https://github.com/jpg-store/vesting-contract/blob/audit/shellcheck/docs/audit/hlint-report.txt) for linting suggestions. 


### Coding Standards - Formatters

| Severity | CVSS | Vulnerability type |
| --- | --- | --- |
| None | [0.0](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator?vector=AV:N/AC:L/PR:N/UI:N/S:C/C:N/I:N/A:N/E:U/RL:X/RC:X/CR:H/IR:H/AR:M/MAV:X/MAC:X/MPR:X/MUI:X/MS:X/MC:N/MI:N/MA:N&version=3.1) | `poor-code-standards` |
#### Description

We recommend the use of standardising code format to minimise change noise, and easier long term maintenance of the code.

#### Recommendation 

Make use of tools like:
- shfmt 
- fourmolu

MLabs team provides the currently open [PR 5](https://github.com/jpg-store/vesting-contract/pull/5) as a POC. 


### Optimisation - Calculating the Total Unvested Value

| Severity | CVSS | Vulnerability type |
| --- | --- | --- |
| None | [0.0](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator?vector=AV:N/AC:L/PR:N/UI:N/S:C/C:N/I:N/A:N/E:U/RL:X/RC:X/CR:H/IR:H/AR:M/MAV:X/MAC:X/MPR:X/MUI:X/MS:X/MC:N/MI:N/MA:N&version=3.1) | None |

#### Description

In the current version, we have to iterate the `Portion` list multiple times to calculate the total [unvested](https://github.com/jpg-store/vesting-contract/blob/main/src/Canonical/Vesting.hs#L177) value:

1. Filter the unvested `Portion` from the given `Schedule`
2. Iterate again to get `amount` from the filtered `Portion`
3. Iterate again to concat all the `Value`(s) present in the list.

```haskell
unvested :: Value
unvested = mconcat . fmap amount . filter (not . isVested) . schedule $ datum
```
The same thing can be accomplished using `foldr` which only requires a single iteration of the list.

```haskell
unvested :: Value
unvested = foldr (\ !portion !totalAmt -> 
                    if isVested portion 
                    then totalAmt
                    else mappend (amount portion) totalAmt
                 ) mempty 
         . schedule $ datum
```


### Recommendation - Improve Naming

| Severity | CVSS | Vulnerability type |
| --- | --- | --- |
| None | [0.0](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator?vector=AV:N/AC:L/PR:N/UI:N/S:C/C:N/I:N/A:N/E:U/RL:X/RC:X/CR:H/IR:H/AR:M/MAV:X/MAC:X/MPR:X/MUI:X/MS:X/MC:N/MI:N/MA:N&version=3.1) | [incorrect-documentation](https://www.notion.so/Incorrect-documentation-66aaf03171a94846b0da63b12787b0bc) |


#### Description

Vesting contract uses `Action` type as it's redeemer, this type has a single data constructor which is named `Disburse` whose type is `Disbure :: [PubKeyHash] -> Action`. We think that this name is misleading as the role of this redeemer is not only to disburse the value locked in the contract when the deadline has passed _but also to update the current beneficiaries of the `Input` datum_. Hence, we think the option of updating beneficiaries should also be highlighted in the name of Action type's data constructor.


\newpage

# Testing Summary

For report brevity (and clarity), the testing framework and associated PRs will be mentioned [here](https://github.com/jpg-store/vesting-contract/issues/21), and not included in the Audit Report. For in-depth explanation about what each PR is aiming to prove, please refer to the linked ticket description on GitHub.

Therefore, we would first mention the tickets outlining the hypotheses that the tests were trying to prove/disprove:

- [_Vesting Contract Onchain Hypotheses_](https://github.com/jpg-store/vesting-contract/issues/9),
- [_Vesting Contract Offchain Hypotheses_](https://github.com/jpg-store/vesting-contract/issues/10).

Secondly, we would like to mention the PRs proposing the tests to be integrated into the main repository. 

- [_Test - Not Enough Signatures_](https://github.com/jpg-store/vesting-contract/issues/19),
- [_Test - Early Withdraw_](https://github.com/jpg-store/vesting-contract/issues/18),
- [_Test - Empty Beneficiaries on Input_](https://github.com/jpg-store/vesting-contract/issues/17),
- [_Test - Empty Beneficiaries on Withdraw_](https://github.com/jpg-store/vesting-contract/issues/16),
- [_Test - Multi User Vesting_](https://github.com/jpg-store/vesting-contract/issues/13),
- [_Test - Native Vesting _](https://github.com/jpg-store/vesting-contract/issues/11),
- [_Test - Happy Path Test Generator_](https://github.com/jpg-store/vesting-contract/issues/6).

We recommend for the aforementioned tests to be included in the main branch and made visible in the CI.


\newpage

# Conclusion

MLabs inspected the onchain and offchain code of the _JPG Vesting Contract_ over a three week period and discovered two vulnerabilities and four recommendations. Additionally, MLabs has provided a testing framework and tests written to verify the claims made in the report (available on GitHub). This list is not exhaustive, as the team only had a limited amount of time to conduct the audit.


\newpage

# Appendix

<!-- Use the tags to link vulnerability types -->

## Vulnerability types

The following list of vulnerability types represents a list of commonly found vulnerabilities in Cardano smart contract protocol designs or implementations. The list of types is actively updated and added to as new vulnerabilities are found.

<a name="other-redeemer"></a>

### Other redeemer

**ID:** other-redeemer

**Test:** Transaction can avoid some checks when it can successfully spend a UTxO or mint a token with a redeemer that some script logic didn’t expect to be used.

**Property:** A validator/policy should check explicitly whether the ‘other’ validator/policy is invoked with the expected redeemer.

**Impacts:**

- Bypassing checks

<a name="other-token-names"></a>

### Other token name

**ID:** other-token-names

**Test:** Transaction can mint additional tokens with some ‘other’ token name of ‘own’ currency alongside the intended token name.

**Property:** A policy should check that the total value minted of their ‘own’ currency symbol doesn’t include unintended token names.

**Impacts:**

- Stealing protocol tokens
- Unauthorised protocol actions

**Example:**

A common coding pattern that introduces such a vulnerability can be observed in the following excerpt:

```haskell
vulnPolicy rmr ctx = do
  …
  assetClassValueOf txInfoMint ownAssetClass == someQuantity
  …
```

The recommended coding pattern to use in order to prevent such a vulnerability can be observed in the following excerpt:

```haskell
safePolicy rmr ctx = do
  …
  txInfoMint == (assetClassValue ownAssetClass someQuantity)
  …
```

<a name="unbounded-protocol-datum"></a>

### Unbounded Protocol datum

**ID:** unbounded-protocol-datum

**Test:** Transaction can create protocol UTxOs with increasingly bigger protocol datums.

**Property:** A protocol should ensure that all protocol datums are bounded within reasonable limits.

**Impacts:**

- Script XU and/or size overflow
- Unspendable outputs
- Protocol halting

**Example:**

A common design pattern that introduces such vulnerability can be observed in the following excerpt:

```haskell
data MyDatum = Foo {
  users :: [String],
  userToPkh :: Map String PubKeyHash
}
```

If the protocol allows these datums to grow indefinitely, eventually XU and/or size limits imposed by the Plutus interpreter will be reached, rendering the output unspendable.

The recommended design patterns is either to limit the growth of such datums in validators/policies or to split the datum across different outputs.

<a name="arbitrary-utxo-datum"></a>

### Arbitrary UTxO datum

**ID:** arbitrary-utxo-datum

**Test:** Transaction can create protocol UTxOs with arbitrary datums.

**Property:** A protocol should ensure that all protocol UTxOs hold intended datums.

**Impacts:**

- Script XU overflow
- Unspendable outputs
- Protocol halting

<a name="unbounded-protocol-value"></a>

### Unbounded protocol value

**ID:** unbounded-protocol-value

**Test:** Transaction can create increasingly more protocol tokens in protocol UTxOs.

**Property:** A protocol should ensure that protocol values held in protocol UTxOs are bounded within reasonable limits.

**Impacts:**

- Script XU overflow
- Unspendable outputs
- Protocol halting

<a name="foreign-utxo-tokens"></a>

### Foreign UTxO tokens

**ID:** foreign-utxo-tokens

**Test:** Transaction can create protocol UTxOs with foreign tokens attached alongside the protocol tokens.

**Property:** A protocol should ensure that protocol UTxOs only hold the tokens used by the protocol.

**Impacts:**

- Script XU overflow
- Unspendable outputs
- Protocol halting

<a name="multiple-satisfaction"></a>

### Multiple satisfaction

**ID:** multiple-satisfaction

**Test:** Transaction can spend multiple UTxOs from a validator by satisfying burning and/or paying requirements for a single input while paying the rest of the unaccounted input value to a foreign address.

**Property:** A validator/policy should ensure that all burning and paying requirements consider all relevant inputs in aggregate.

**Impacts:**

- Stealing protocol tokens
- Unauthorised protocol actions
- Integrity

**Example:**

A common coding pattern that introduces such a vulnerability can be observed in the following excerpt:

```haskell
vulnValidator _ _ ctx =
  ownInput ← findOwnInput ctx
  ownOutput ← findContinuingOutput ctx
  traceIfFalse “Must continue tokens” (valueIn ownInput == valueIn ownOutput)
```

Imagine two outputs at `vulnValidator` holding the same values

A. `TxOut ($FOO x 1 + $ADA x 2)` B. `TxOut ($FOO x 1 + $ADA x 2)`

A transaction that spends both of these outputs can steal value from one spent output by simply paying `$FOO x 1 + $ADA x 2` to the ‘correct’ address of the `vulnValidator`, and paying the rest `$FOO x 1 + $ADA x 2` to an arbitrary address.

<a name="locked-ada"></a>

### Locked Ada

**ID:** locked-ada

**Test:** Protocol locks Ada value indefinitely in obsolete validator outputs.

**Property:** Protocol should include mechanisms to enable redeeming any Ada value stored at obsolete validator outputs.

**Impacts:**

- Financial sustainability
- Cardano halting

<a name="locked-nonada-values"></a>

### Locked non Ada values

**ID:** locked-nonada-values

**Test:** Protocol indefinitely locks some non-Ada values that ought to be circulating in the economy.

**Property:** Protocol should include mechanisms to enable redeeming any non-Ada value stored at obsolete validator outputs.

**Impacts:**

- Financial sustainability
- Protocol halting

<a name="missing-utxo-authentication"></a>

### Missing UTxO authentication

**ID:** missing-utxo-authentication

**Test:** Transaction can perform a protocol action by spending or referencing an illegitimate output of a protocol validator.

**Property:** All spending and referencing of protocol outputs should be authenticated.

**Impacts:**

- Unauthorised protocol actions

**Example:**

Checking only for validator address and not checking for an authentication token.,

<a name="missing-incentive"></a>

### Missing incentive

**ID:** missing-incentive

**Test:** There is no incentive for users to participate in the protocol to maintain the intended goals of the protocol.

**Property:** All users in the Protocol should have an incentive to maintain the intended goals of the protocol

**Impacts:**

- Protocol stalling
- Protocol halting

<a name="bad-incentive"></a>

### Bad incentive

**ID:** bad-incentive

**Test:** There is an incentive for users to participate in the protocol that compromises the intended goals of the protocol.

**Property:** No users of the protocol should have an incentive to compromise the intended goals of the protocol.

**Impacts:**

- Protocol stalling
- Protocol halting

<a name="utxo-contention"></a>

### UTxO contention

**ID:** utxo-contention

**Test:** The protocol requires that transactions spend a globally shared UTxO(s) thereby introducing a contention point.

**Property:** The protocol should enable parallel transactions and contention-less global state management if possible.

**Impacts:**

- Protocol stalling
- Protocol halting

<a name="cheap-spam"></a>

### Cheap spam

**ID:** cheap-spam

**Test:** A transaction can introduce an idempotent or useless action/effect in the protocol for a low cost that can compromise protocol operations.

**Property:** The protocol should ensure that the cost for introducing a salient action is sufficient to deter spamming.

Severity increases when compounded with the `utxo-contention` vulnerability.

**Impacts:**

- Protocol stalling
- Protocol halting

<a name="insufficient-tests"></a>

### Insufficient tests

**ID:** insufficient-tests

**Test:** There is piece of validation logic that tests do not attempt to verify.

**Property:** Every piece of validator code gets meaningfully executed during tests.

**Impacts:**

- Correctness

<a name="incorrect-documentation"></a>

### Incorrect documentation

**ID:** incorrect-documentation

**Test:** There is a mistake or something confusing in existing documentation.

**Property:** Everything documented is clear and correct.

**Impacts:**

- Correctness
- Maintainability

<a name="insufficient-documentation"></a>

### Insufficient documentation

**ID:** insufficient-documentation

**Test:** There is a lack of important documentation.

**Property:** Everything of importance is documented.

**Impacts:**

- Comprehension
- Correctness

<a name="poor-code-standards"></a>

### Poor Code Standards

**ID:** poor-code-standards

**Test:** Missing the use of code quality and stadardisation tools. 

**Property:** Code is properly formatted, linted, and uses an adequate code standard.

**Impacts:**

- Codebase Maintainability 
- Comprehension


