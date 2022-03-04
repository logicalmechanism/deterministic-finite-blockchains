# Building

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
echo "done"
```