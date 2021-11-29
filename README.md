
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

```bash
cd cardano-node
./testnet-node-local/bin/cardano-node-testnet
```
```bash
# gets socket on path
CARDANO_NODE_SOCKET_PATH=../cardano-node/state-node-testnet/node.socket
../cardano-node/cardano-cli-testnet/bin/cardano-cli query tip --testnet-magic 1097911063
```

```bash
cabal clean
cabal build -w ghc-8.10.4
cabal run dfb-contract
echo "done"
```

```bash
# First Terminal
cd plutus
git checkout plutus-pab/v0.0.2
. /home/cardano/.nix-profile/etc/profile.d/nix.sh
nix-shell
cd plutus-playground-client
. /home/cardano/.nix-profile/etc/profile.d/nix.sh
plutus-playground-server

# Second Terminal
cd plutus
git checkout plutus-pab/v0.0.2
. /home/cardano/.nix-profile/etc/profile.d/nix.sh
nix-shell
cd plutus-playground-client
. /home/cardano/.nix-profile/etc/profile.d/nix.sh
npm run start
```