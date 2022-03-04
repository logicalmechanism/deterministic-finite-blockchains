
def initializeWallets(players:  int, tokens:int) -> dict:
    """
    The wallet object will be object of objects

    >>> initializeWallets(4, 2)
    {0: {0: 0, 1: 0}, 1: {0: 0, 1: 0}, 2: {0: 0, 1: 0}, 3: {0: 0, 1: 0}}
    """
    wallets = {}
    for i in range(players):
        wallets[i] = {}
        for t in range(tokens):
            wallets[i][t] = 0
    return wallets

def reserves(wallets, tokens):
    """
    Calculate the reserves of the wallets.

    >>> wallets = initializeWallets(3, 2)
    >>> wallets[0][1] = 100
    >>> wallets[1][1] = 10
    >>> wallets[1][0] = 50
    >>> reserves(wallets, 2)
    {0: 50, 1: 110}
    """
    reserve = {}
    for t in range(tokens):
        reserve[t] = 0
    for user in wallets:
        for token in wallets[user]:
            reserve[token] += wallets[user][token]
    return reserve


def stringify(wallets):
    """
    >>> wallets = initializeWallets(3, 2)
    >>> wallets[0][1] = 100
    >>> wallets[1][1] = 10
    >>> wallets[1][0] = 50
    >>> wallets
    {0: {0: 0, 1: 100}, 1: {0: 50, 1: 10}, 2: {0: 0, 1: 0}}
    >>> stringify(wallets)
    {'0': {'0': 0, '1': 100}, '1': {'0': 0, '1': 10}, '2': {'0': 0, '1': 0}}
    """
    toData = {}
    for w in wallets:
        sw = str(w)
        toData[sw] = {}
        for t in wallets[w]:
            st = str(t)
            if t == 0:
                toData[sw][st] = 0
            else:
                toData[sw][st] = wallets[w][t]
    return toData



if "__main__" == __name__:
    import doctest
    doctest.testmod()