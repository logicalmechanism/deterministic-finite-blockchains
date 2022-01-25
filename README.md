# Deterministic Finite Blockchains

```
This code is a plutus implementation for the DFB based off a python prototype.
```

Please refer to the [Deterministic Finite Blockchain GitBook](https://ancientkraken.gitbook.io/deterministic-finite-blockchains/) for more information.

## Cloning from GitHub
When cloning the repo be sure to use the recursive submodule parameter with git clone to get all the required contract files.

```
git clone --recurse-submodules https://github.com/logicalmechanism/deterministic-finite-blockchains
```

# The code requirements

```
cardano-cli version
# cardano-cli 1.33.0 - linux-x86_64 - ghc-8.10
# git rev 814df2c146f5d56f8c35a681fe75e85b905aed5d

cardano-node version
#cardano-node 1.33.0 - linux-x86_64 - ghc-8.10
#git rev 814df2c146f5d56f8c35a681fe75e85b905aed5d

cabal --version
# cabal-install version 3.4.0.0
# compiled using version 3.4.0.0 of the Cabal library

ghc --version
# The Glorious Glasgow Haskell Compilation System, version 8.10.4
```