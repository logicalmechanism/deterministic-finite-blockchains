## Helper Code

Getting the testnet socket on path and using it with a cli command.
```bash
# gets socket on path
CARDANO_NODE_SOCKET_PATH=path/tp/the/testnet/node.socket
cardano-cli query tip --testnet-magic 1097911063
```

Build the smart contract inside the dfb-contract folder.

```bash
# Clean and Build, this take a long time.
rm dfb_contract.plutus
cabal clean
cabal build -w ghc-8.10.4
cabal run dfb-contract
echo "done"
```

```bash
# Quick Build
cabal build -w ghc-8.10.4
cabal run dfb-contract
echo "done"
```