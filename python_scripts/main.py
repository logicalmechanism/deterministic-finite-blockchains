from offchain    import OffChain
from dfbchain    import DFB
from transaction import Tx
from sys         import exit
import click
import os

# This will need to be a function that hashes the script
script_address = 'addr_test1wzya4mwk37mfd6k9t5e0qk62pvy0w2c4zvmqd959kta563q86w636'
###############################################################################
# Requires Blockfrost API Key.
try:
    with open("blockfrost_api.key", "r") as read_content: api_key = read_content.read().splitlines()[1]
except IndexError:
    print('Error: Incorrect Blockfrost API Key')
    exit(1)
###############################################################################

def run_dfb():
    oc = OffChain(api_key, script_address)
    print("Searching for contract data")
    oc.find_everything()
    # This is the list of "wallets" inside the sc
    wallets = oc.addr_curr_map
    synthetic_asset = {
        'unit': 'synthetic_asset',
        'quantity': 0
    }
    for addr in wallets:
        wallets.update({addr: wallets[addr]+[synthetic_asset]})
    currency = oc.currency
    if bool(currency) is False:
        print("Error: No UTxOs Inside Smart Contract")
        exit(1)
    currency['synthetic_asset'] = 0
    # print(wallets)
    block_hash = oc.block_hash
    # create DFB IC
    dfb = DFB(wallets, currency, block_hash)
    # for wallet in dfb.wallets:
    #     print(dfb.wallets[wallet])
    # # Run DFB
    print("Beginning Game")
    print(f"Initial Hash: {block_hash}")
    print("Rate: {} Pool: {}".format(dfb.mining_rate, dfb.mining_pool))
    while dfb.extend is True:
        dfb.mining()
        dfb.trading()
        dfb.increment_block_number()
    print('Game Ended: ', dfb.block_number)
    largest = 0
    winner = ''
    for wallet in dfb.wallets:
        print('\n',wallet)
        for value in dfb.wallets[wallet]:
            print(value)
        if largest < dfb.wallets[wallet][2]['quantity']:
            largest = dfb.wallets[wallet][2]['quantity']
            winner = wallet
    print('\nwinner', winner, largest)
    # # Build outgoing transactions with DFB output
    tx = Tx(False, script_address)
    tx.run_game('../wallets/wallet_09/') # This wallet will be the miner

@click.group()
def main():
    """
    Simple CLI.
    """
    pass

@main.command()
def start():
    """Start endpoint"""
    click.echo('Starting Game')
    tx = Tx(False, script_address)
    tx.start_game()

@main.command()
def run():
    """Start endpoint"""
    click.echo('Running Game')
    run_dfb()

@main.command()
@click.argument('wallet_path')
def abandon(wallet_path):
    """Start endpoint"""
    click.echo(f'Abandoning Game for wallet {wallet_path}')
    tx = Tx(False, script_address)
    tx.send_from_sc_into_wallet(wallet_path)

@main.command()
def balances():
    click.echo("Printing all balances")
    oc = OffChain(api_key, script_address)
    print('\nContract Address:', script_address)
    oc.print_all_currency_at_address(script_address)
    wallet_path = '../wallets/'
    for root, dirs, files in os.walk(wallet_path, topdown=False):
        for name in dirs:
            wallet_path = os.path.join(root, name)+'/payment.addr'
            with open(wallet_path, 'r') as f:
                address = f.readline()
                print('\n', wallet_path)
                print('Address:', address)
                oc.print_all_currency_at_address(address)


if __name__ == '__main__':
    # python main.py search test
    # test
    main()


    