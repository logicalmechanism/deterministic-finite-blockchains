from helper import bq, b10, getHash, reduction, pairs, hashToNumber, string_to_list_in_pairs
from wallets import initializeWallets, stringify
from mining import miningPhase
from trading import tradingPhase
from checking import checkPhase
import pprint
from sys import exit
def main():
    """
    run the program.

    TEST
    """
    N = 6
    # print('Players: {}\n'.format(N))
    tokens = 5 # 4 + 1 synthetic
    # print('Tokens: {}\n'.format(tokens-1))

    # Mining pool
    mining_rate = 100
    halving = 1*mining_rate
    miners = len(bq(b10(getHash(N)), N))+2
    # |400*log(N)/N|
    mining_pool = miners*mining_rate*halving*2
    # exit()
    # print('The Mining Pool: {}\n'.format(mining_pool))
    
    #
    block_number = 0
    block_string = "this is a string for you and me but most for them."
    block_hash = getHash(block_string)


    # Create the DFB Wallets From real world values.
    wallets = initializeWallets(N, tokens)
    # This is for testing
    wallets[0][1]=102034
    wallets[1][2]=13842
    wallets[2][3]=56134
    wallets[3][4]=3134

    # pprint.pprint(wallets)
    wallet_hash = getHash(str(wallets))

    # for finding low and high of dfb
    low = 0
    prevLow = 0
    high = 0
    prevHigh = 0
    prev = (block_string, wallet_hash)

    # Run the Chain
    while True:

        # Mining
        number = b10(block_hash)
        mining_list = bq(number, N)
        miner_tokens = bq(number, tokens)
        wallets, mining_pool = miningPhase(mining_list, wallets, mining_rate, mining_pool)

        # Trading
        number_hash = getHash(number)
        number = b10(number_hash)
        amount = reduction(number)
        transaction_list = bq(number, N)
        traders_tokens = bq(number, tokens) # Trading is betwen pairs.
        trading_pairs = pairs(transaction_list, mining_list)
        token_pairs = pairs(traders_tokens, miner_tokens)
        # fix list lengths
        if len(trading_pairs) >= len(token_pairs):
            trading_pairs = trading_pairs[:len(token_pairs)]
        else:
            token_pairs = token_pairs[:len(trading_pairs)]
        wallets, successful_trade_flag = tradingPhase(trading_pairs, token_pairs, wallets, tokens, amount)

        # Checking
        ending_flag, mining_rate = checkPhase(block_number, halving, mining_pool, mining_rate, successful_trade_flag)
        if ending_flag is False:
            # End the chain
            break
        else:
            block_number += 1
            
            # Find the low and high of the dfb.
            check = b10(block_hash)
            if b10(low) == 0:
                low = block_hash
                prevLow = prev
            
            if check < b10(low):
                low = block_hash
                prevLow = prev
            
            if check > b10(high):
                high = block_hash
                prevHigh = prev

            # increment
            wallet_hash = getHash(str(wallets))
            prev = (block_hash, wallet_hash)
            block_string = block_hash + wallet_hash
            block_hash = getHash(block_string)

    # print('The final block number is {}'.format(block_number)) #
    # This will become the new starting phrase for the next dfb.
    print('The final block hash: {}'.format(block_hash))
    print()

    # This gets stored in the metadata
    toData = stringify(wallets)
    pprint.pprint(toData)

    # The final wallet proof
    wallet_hash = getHash(str(toData))
    wallet_proof = getHash(block_hash + wallet_hash)
    print()

    # This is the validation requirements.
    # "somestring".encode('utf-8').hex() # hex string
    try:
        # print('wallet : {} -> {}'.format((block_hash.encode('utf-8').hex(), wallet_hash.encode('utf-8').hex()), wallet_proof))

        # print('wallet : {} -> {}'.format((block_hash, wallet_hash), hashToNumber(wallet_proof)))
        print('low    : {} -> {}'.format(prevLow, hashToNumber(low)))
        print('high   : {} -> {}'.format(prevHigh, hashToNumber(high)))
        print('last   : {} -> {}'.format(prev, hashToNumber(block_hash)))
    except TypeError:
        pass

if "__main__" == __name__:
    main()