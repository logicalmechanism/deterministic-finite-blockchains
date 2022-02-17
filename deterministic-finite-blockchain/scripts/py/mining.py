from typing import Tuple

def miningPhase(mining_list: list, wallets: dict, mining_rate: int, mining_pool: int) -> Tuple[dict, int]:
    """
    Mine the synthetic token into the wallet.

    >>> x = {0:{0:0},1:{0:0}}
    >>> wallets, pool = mining([1,0,0,1], x, 1, 1000)
    >>> wallets
    {0: {0: 2}, 1: {0: 2}}
    """
    for m in mining_list:
        if mining_pool <= 0:
            mining_pool = 0
            break
        if mining_pool < mining_rate:
            mining_rate = mining_pool
        wallets[m][0] += mining_rate
        mining_pool -= mining_rate
    return wallets, mining_pool


if "__main__" == __name__:
    import doctest
    doctest.testmod()