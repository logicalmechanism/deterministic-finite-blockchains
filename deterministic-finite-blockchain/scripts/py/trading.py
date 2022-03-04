from wallets import reserves


def product(reserve:dict, ignore:list) -> int:
    """
    Calculate the product of the reserves with options ignore list.

    >>> product({}, [])
    1

    >>> product({0: 50, 1:100, 2:75}, [])
    77.885784716188
    
    >>> product({0: 50, 1:100, 2:75}, [0,1])
    4.217163326508746
    """
    total = 1
    for token in reserve:
        if token not in ignore:
            if reserve[token] > 0:
                total *= pow(reserve[token], weight(reserve, token))
    return total


def weight(reserve: dict, token: int):
    """
    Calculate the tokens weight.

    >>> weight({0: 50, 1:100, 2:75}, 2)
    0.3333333333333333
    """
    return reserve[token] / sum(reserve.values())


def calculate_amount(reserve, tokenA, tokenB, deltaA):
    """
    Calculate the deltaB given deltaA.

    >>> calculate_amount({0: 105, 1:210, 2:175}, 2, 0, 35)
    47
    """
    k = product(reserve, [])
    c = product(reserve, [tokenA, tokenB])
    weightA = weight(reserve, tokenA)
    weightB = weight(reserve, tokenB)

    reserveA = reserve[tokenA]
    reserveB = reserve[tokenB]

    deltaB = pow(k/(c*pow(reserveA - deltaA, weightA)), 1/weightB) - reserveB
    try:
        return int(deltaB)
    except TypeError:
        return 0


def tradingPhase(trading_pairs, token_pairs, wallets, tokens, deltaA):
    """
    """
    flag = False # check if any trade occurs
    for trade, token in zip(trading_pairs, token_pairs):
        A = wallets[trade[0]][token[0]]
        B = wallets[trade[1]][token[1]]
        reserve = reserves(wallets, tokens)
        deltaB = calculate_amount(reserve, token[0], token[1], deltaA)
        # print(trade, token, deltaA, deltaB)
        if (A-deltaA > 0) and (B - deltaB > 0) and (deltaB > 0):
            flag = True
            wallets[trade[0]][token[0]] = A - deltaA
            wallets[trade[0]][token[1]] += deltaB
            wallets[trade[1]][token[1]] = B - deltaB
            wallets[trade[1]][token[0]] += deltaA
    return wallets, flag


if "__main__" == __name__:
    import doctest
    doctest.testmod()