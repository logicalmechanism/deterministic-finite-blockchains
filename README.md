# Deterministic Finite Blockchains

```
This code is a python off-chain implementation for the DFB.
```

The contract will start simple without endpoints so the off-chain code can be tested.

Additional endpoints will be added after basic functionality has been tested.

## Helper Code

For updating the cardano node
```bash
cd cardano-node
git fetch --all --recurse-submodules --tags
git checkout $(curl -s https://api.github.com/repos/input-output-hk/cardano-node/releases/latest | jq -r .tag_name)
. /home/cardano/.nix-profile/etc/profile.d/nix.sh
nix-build -A scripts.testnet.node -o testnet-node-local
nix-build -A cardano-cli -o cardano-cli-testnet
./cardano-cli-testnet/bin/cardano-cli --version
./testnet-node-local/bin/cardano-node-testnet
```

Running the cardano testnet
```bash
cd cardano-node
./testnet-node-local/bin/cardano-node-testnet
```

Getting the testnet socket on path
```bash
# gets socket on path
CARDANO_NODE_SOCKET_PATH=../cardano-node/state-node-testnet/node.socket
../cardano-node/cardano-cli-testnet/bin/cardano-cli query tip --testnet-magic 1097911063
```

Building the smart contract
```bash
cabal clean
cabal build -w ghc-8.10.4
cabal run dfb-contract
echo "done"
```