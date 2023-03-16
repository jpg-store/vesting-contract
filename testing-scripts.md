
## Running the scripts using plutip's local-cluster
  
  This is quick tutorial on how to setup wallets and config to run the shell scripts for vesting contract.


1. Spin up plutip's local-cluster.

  ```bash
      local-cluster --wallets 5 --wallet-dir ./tmp
  ```
  wait for it to initialize the wallets, after wallets has been initialized you will see something like this:

  ```
  Starting cluster...
  Chain index started at port 9083
  Waiting for wallets to be funded...
  Waiting till all wallets will be funded...

  ------------

  Wallet 1 PKH: c1a507ac9c53dab7f24ff25ea727ea9bb727ecb7ad621c1c5f445d43
  Wallet 1 mainnet address: "addr1v8q62pavn3fa4dljfle9afe8a2dmwflvk7kky8qutaz96scja22q0"
  Wallet 2 PKH: 5da4dcc87b0a3a85ffb969215aac4369f93de9673d8031c4ae43abcf
  Wallet 2 mainnet address: "addr1v9w6fhxg0v9r4p0lh95jzk4vgd5lj00fvu7cqvwy4ep6hnc9kphlp"
  Wallet 3 PKH: e568ca1e424ff4d19b769f953158b964f926ffb03961921482bbb9a2
  Wallet 3 mainnet address: "addr1v8jk3js7gf8lf5vmw60e2v2ch9j0jfhlkqukrys5s2amngsehkf2d"
  Wallet 4 PKH: c65aaec4ebe5f198c3b566ce89b8c45a9580e315779d1fddff28a32d
  Wallet 4 mainnet address: "addr1v8r94tkya0jlrxxrk4nvazdcc3dftq8rz4me687alu52xtgg9q7zf"
  Wallet 5 PKH: b17a4270fc89e36fe1014805cd6890f436aa36a4bb6628b90f6b6731
  Wallet 5 mainnet address: "addr1vxch5snsljy7xmlpq9yqtntgjr6rd23k5jakv29epa4kwvg6qe9mr"
  Node socket: CardanoNodeConn "/tmp/nix-shell.kp3vJ8/test-cluster1278596/pool-1/node.socket"

  ------------

  Cluster is running. Ctrl-C to stop.
  ```


2. export node socket path and run

    ```bash
      source scripts/envars/local-cluster.envvars
    ```
3. update the wallets pubkeyhashes, address and skey present in ./temp/mainnet/. This has to be done manually i.e copy PKH shown in the output of local-cluster and update it in all the files.
   For Eg:

   ```bash
    echo "c1a507ac9c53dab7f24ff25ea727ea9bb727ecb7ad621c1c5f445d43" > ./temp/mainnet/benefactor-pkh.txt
   ```

   The skey of the wallets can be found in ./tmp dir created by plutip.

4. run
  ```bash
    cd scripts/happy-path && ./lock-tx.sh lock
  ```

  Now the transaction should be successfully submitted.

5. Now run,
  ```bash
    cardano-cli query utxo --address $(cat ../../assets/mainnet/vesting.addr) --mainnet
  ```
  to check for the submitted utxo.
   
