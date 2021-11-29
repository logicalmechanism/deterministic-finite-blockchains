from offchain import OffChain
from dfbchain import DFB
from sys import exit

def run_dfb():
    # Requires Blockfrost API Key.
    try:
        with open("blockfrost_api.key", "r") as read_content: api_key = read_content.read().splitlines()[1]
    except IndexError:
        print('Error: Incorrect Blockfrost API Key')
        exit(1)
    script_address = 'addr_test1wzya4mwk37mfd6k9t5e0qk62pvy0w2c4zvmqd959kta563q86w636'
    oc = OffChain(api_key, script_address)
    oc.find_everything()
    # This is the list of "wallets" inside the sc
    wallets = oc.addr_curr_map
    currency = oc.currency
    if bool(currency) is False:
        print("Error: No UTxOs Inside Smart Contract")
        exit(1)
    print(currency)
    block_hash = oc.block_hash
    # create DFB IC
    dfb = DFB(wallets, currency, block_hash)
    print(dfb.reserves)
    # Run DFB
    while dfb.extend is True:
        dfb.mining()
        dfb.trading()
        dfb.increment_block_number()
    # Build outgoing transactions with DFB output
    print(dfb.wallets)


if __name__ == "__main__":
    run_dfb()

    