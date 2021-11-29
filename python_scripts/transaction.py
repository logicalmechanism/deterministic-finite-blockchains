import os
from pyccli import query, address, trx, helper
from sys import exit
"""
"""
class Tx:

    # Data that needs to be stored.
    def __init__(self, mainnet_flag: bool, script_address: str) -> None:
        self.outgoing = []
        self.incoming = []
        self.mainnet  = mainnet_flag
        self.address  = script_address
    
    def send_from_wallet_into_sc(self, wallet_path) -> bool:
        """
        Send all the assets from a wallet into the smart contract.
        Return a bool upon completion.

        >>> Tx(False, "abc").send_from_wallet_into_sc('')
        False

        >>> Tx(False, "addr_test1").send_from_wallet_into_sc('../wallets/wallet_01/')
        False
        """
        if os.path.isdir(wallet_path) is False:
            return False
        query.protocol_parameters('tmp/', mainnet_flag=self.mainnet)
        query.tip('tmp/', mainnet_flag=self.mainnet)
        data, err = helper.json_open('tmp/', 'tip.json')
        final_tip = data['slot'] + 25000
        addr, err = address.build(wallet_path, 'payment.vkey', mainnet_flag=self.mainnet)
        query.utxo(addr, 'tmp/', mainnet_flag=self.mainnet)
        data, err = helper.json_open('tmp/', 'utxo.json')
        datum_hash, err = trx.hash_script_data('../datum_redeemer/datum.json')
        txin = []
        currency_in_wallet = {}
        for tx in data:
            txin += ['--tx-in', tx]
            value = data[tx]['value']
            for unit in value:
                try:
                    currency_in_wallet[unit] += value[unit]
                except:
                    currency_in_wallet[unit] = value[unit]
        out_value = ''
        for token in currency_in_wallet:
            if token == 'lovelace':
                continue
            for asset in currency_in_wallet[token]:
                out_value += str(currency_in_wallet[token][asset])+' '+token+'.'+asset
                out_value += ' + '
        out_value = out_value[:-3]
        if out_value == '':
            return False
        additional_options = [
            '--tx-out-datum-hash', datum_hash
        ]
        minimum_utxo, errs = trx.calculate_min_value('tmp/','protocol_parameters.json', self.address, out_value, additional_options)
        if errs == 1:
            return False
        txout = self.address +' + '+ minimum_utxo.split(' ')[1] + ' + ' + out_value
        utxo_in_out = txin + [
            '--tx-out', txout,
            '--tx-out-datum-hash', datum_hash
        ]
        trx.build('tmp/protocol_parameters.json', 'tmp/tx.draft', addr, final_tip, utxo_in_out, [], mainnet_flag=self.mainnet)
        signers = [
            '--signing-key-file',
            wallet_path+'payment.skey'
        ]
        trx.sign('tmp/tx.draft', 'tmp/tx.signed', signers, mainnet_flag=self.mainnet)
        p, e = trx.submit('tmp/', 'tx.signed', mainnet_flag=self.mainnet)
        print(p, e)
        # Get wallet utxos
        # build trx, sign submit
        return True


    
    def start_game(self):
        """
        Auto start a game with the wallets folder.

        >>> Tx(False, "addr_test1wzya4mwk37mfd6k9t5e0qk62pvy0w2c4zvmqd959kta563q86w636").start_game()
        """
        # design function to spend a single wallet into the sc
        # loop the function for all the wallets in the folder
        wallet_path = '../wallets/'
        for root, dirs, files in os.walk(wallet_path, topdown=False):
            for name in dirs:
                wallet_path = os.path.join(root, name)+'/'
                self.send_from_wallet_into_sc(wallet_path)
        

    def run_game(self):
        pass

    def abandon_game(self):
        pass

if __name__ == '__main__':
    import doctest
    doctest.testmod()
    
