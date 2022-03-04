from helper import getHash
import json

def metadataObject(datumHash, tokens, currentWallets, suggestedWallets):
    data = {
        "1731": {
            "datumHash": datumHash,
            "tokens": tokens,
            "currentWallets": currentWallets,
            "suggestedWallets": suggestedWallets
        }
    }
    return data


def writeData(datumHash, tokens, currentWallets, suggestedWallets):
    """
    Write the 1731 metadata into metadata.json
    >>> dh = "6c11f4db0358ff56993bc1d20b20d54ebbc6886ab211c7cfbc52f45765956684"
    >>> tokens = {"1": {"policyid1": "assetname1"},"2": {"policyid1": "assetname2"},"3": {"policyid2": "assetname1"}}
    >>> wallets = {"0": {"0": 0, "1": 1359, "2": 257, "3": 1345}}
    >>> writeData(dh, tokens, wallets, {})

    """
    with open('../data/metadata.json', 'w', encoding='utf-8') as f:
        json.dump(metadataObject(datumHash, tokens, currentWallets, suggestedWallets), f)


def metadataHash(object):
    """
    >>> tokens = {"1": {"policyid1": "assetname1"},"2": {"policyid1": "assetname2"},"3": {"policyid2": "assetname1"}}
    >>> wallets = {"0": {"0": 0, "1": 1359, "2": 257,   "3": 1345},"1": {"0": 0, "1": 3147, "2": 10077, "3": 135},"2": {"0": 0, "1": 4085, "2": 8059,  "3": 545},"3": {"0": 0, "1": 5251, "2": 78641, "3": 645}}
    >>> obj = metadataObject('af',tokens, wallets, {})
    >>> metadataHash(obj)
    """
    return getHash(object)

if "__main__" == __name__:
    import doctest
    doctest.testmod()