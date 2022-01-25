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
    
    
    def update_datum_file(self, wallet_path: str) -> None:
        """
        Update the datum file with the correct pkh value.
        """
        new_datum_value, err = trx.public_key_hash(wallet_path+'payment.vkey')
        current_datum_data, err = helper.json_open('../datum_redeemer/', 'datum.json')
        current_datum_data['fields'][0]['bytes'] = new_datum_value
        helper.json_save(current_datum_data, '../datum_redeemer/', 'datum.json')

    
    def send_from_sc_into_wallet(self, wallet_path: str) -> bool:
        """
        Send a specific asset from the smart contract into the wallet.
        """
        if os.path.isdir(wallet_path) is False:
            return False
        # Get Parameters
        query.protocol_parameters('tmp/', mainnet_flag=self.mainnet)
        query.tip('tmp/', mainnet_flag=self.mainnet)
        tip_data, _ = helper.json_open('tmp/', 'tip.json')
        final_tip = tip_data['slot'] + 25000
        # Get wallet address
        wallet_addr, _ = address.build(wallet_path, 'payment.vkey', mainnet_flag=self.mainnet)
        query.utxo(wallet_addr, 'tmp/', mainnet_flag=self.mainnet)
        collateral_data, _ = helper.json_open('tmp/', 'utxo.json')
        collateral_txin, _ = self.get_all_currency(collateral_data)
        # Update Datum
        self.update_datum_file(wallet_path)
        datum_hash, err = trx.hash_script_data('../datum_redeemer/datum.json')
        # Get Script UTxOs
        query.utxo(self.address, 'tmp/', mainnet_flag=self.mainnet)
        utxo_data, _ = helper.json_open('tmp/', 'utxo.json')
        txin, currency_in_wallet = self.get_all_currency(utxo_data, datum_hash)
        # create outbound value
        out_value = self.create_outbound_value_string(currency_in_wallet)
        if out_value == '':
            print('Error: No Values')
            return False
        # calculate min value
        minimum_utxo, errs = trx.calculate_min_value('tmp/','protocol_parameters.json', self.address, out_value, [])
        if errs == 1:
            return False
        txout = wallet_addr +' + '+ minimum_utxo.split(' ')[1] + ' + ' + out_value
        utxo_in_out = collateral_txin+txin + [
            '--tx-out', txout,
            '--tx-in-datum-file', '../datum_redeemer/datum.json',
            '--tx-in-redeemer-file', '../datum_redeemer/redeemer.json',
            '--tx-in-script-file', '../compiled_plutus/dfb_contract.plutus'
        ]
        collateral = ['--tx-in-collateral' if x == '--tx-in' else x for x in collateral_txin]
        p,e = trx.build('tmp/protocol_parameters.json', 'tmp/tx.draft', wallet_addr, final_tip, utxo_in_out, collateral, mainnet_flag=self.mainnet)
        signers = [
            '--signing-key-file',
            wallet_path+'payment.skey'
        ]
        trx.sign('tmp/tx.draft', 'tmp/tx.signed', signers, mainnet_flag=self.mainnet)
        p, e = trx.submit('tmp/', 'tx.signed', mainnet_flag=self.mainnet)
        print(p)
        if e == 1:
            return False
        return True
    
    def get_all_currency(self, utxo_data, datum_hash=''):
        txin = []
        currency_in_wallet = {}
        for tx in utxo_data:
            if datum_hash != '':
                if utxo_data[tx]['datumhash'] == datum_hash:
                    txin += ['--tx-in', tx]
            else:
                txin += ['--tx-in', tx]
            value = utxo_data[tx]['value']
            if datum_hash != '':
                if utxo_data[tx]['datumhash'] == datum_hash:
                    for unit in value:
                        try:
                            currency_in_wallet[unit] += value[unit]
                        except:
                            currency_in_wallet[unit] = value[unit]
            else:
                for unit in value:
                    try:
                        currency_in_wallet[unit] += value[unit]
                    except:
                        currency_in_wallet[unit] = value[unit]
        return txin, currency_in_wallet

    
    def create_outbound_value_string(self, currency_in_wallet: dict) -> str:
        out_value = ''
        for token in currency_in_wallet:
            if token == 'lovelace':
                continue
            for asset in currency_in_wallet[token]:
                out_value += str(currency_in_wallet[token][asset])+' '+token+'.'+asset
                out_value += ' + '
        out_value = out_value[:-3]
        return out_value


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
        
        # Get Parameters
        query.protocol_parameters('tmp/', mainnet_flag=self.mainnet)
        query.tip('tmp/', mainnet_flag=self.mainnet)
        tip_data, err = helper.json_open('tmp/', 'tip.json')
        final_tip = tip_data['slot'] + 25000
        # Build the wallet address
        addr, err = address.build(wallet_path, 'payment.vkey', mainnet_flag=self.mainnet)
        # get wallet utxos
        query.utxo(addr, 'tmp/', mainnet_flag=self.mainnet)
        utxo_data, err = helper.json_open('tmp/', 'utxo.json')
        # Update datum valuee
        self.update_datum_file(wallet_path)
        datum_hash, err = trx.hash_script_data('../datum_redeemer/datum.json')
        txin, currency_in_wallet = self.get_all_currency(utxo_data)
        out_value = self.create_outbound_value_string(currency_in_wallet)
        if out_value == '':
            return False
        additional_options = [
            '--tx-out-datum-hash', datum_hash
        ]
        minimum_utxo, errs = trx.calculate_min_value('tmp/','protocol_parameters.json', self.address, out_value, additional_options)
        if errs == 1:
            return False
        min_ada = 2*int(minimum_utxo.split(' ')[1])
        txout = self.address +' + '+ str(min_ada) + ' + ' + out_value
        utxo_in_out = txin + [
            '--tx-out', txout,
            '--tx-out-datum-hash', datum_hash
        ]
        print('\n', wallet_path)
        print(datum_hash)
        print(txout)
        trx.build('tmp/protocol_parameters.json', 'tmp/tx.draft', addr, final_tip, utxo_in_out, [], mainnet_flag=self.mainnet)
        signers = [
            '--signing-key-file',
            wallet_path+'payment.skey'
        ]
        trx.sign('tmp/tx.draft', 'tmp/tx.signed', signers, mainnet_flag=self.mainnet)
        p, e = trx.submit('tmp/', 'tx.signed', mainnet_flag=self.mainnet)
        print(p)
        if e == 1:
            return False
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


    def send_from_sc_to_sc(self, wallet_path: str):
        """
        Send all the assets in a game back into the sc with updated values.
        """
        if os.path.isdir(wallet_path) is False:
            return False
        # Get Parameters
        query.protocol_parameters('tmp/', mainnet_flag=self.mainnet)
        query.tip('tmp/', mainnet_flag=self.mainnet)
        tip_data, err = helper.json_open('tmp/', 'tip.json')
        final_tip = tip_data['slot'] + 25000
        # Build the wallet address
        miner_addr, err = address.build(wallet_path, 'payment.vkey', mainnet_flag=self.mainnet)
        # get wallet utxos of the miner
        query.utxo(miner_addr, 'tmp/', mainnet_flag=self.mainnet)
        miner_utxo_data, err = helper.json_open('tmp/', 'utxo.json')
        miner_txin, currency_in_miner_wallet = self.get_all_currency(miner_utxo_data)
        # Update datum valuee
        # Loop all the wallets and test each datum file
        self.update_datum_file(wallet_path)
        datum_hash, err = trx.hash_script_data('../datum_redeemer/datum.json')
        # Script data for a single datum hash
        query.utxo(self.address, 'tmp/', mainnet_flag=self.mainnet)
        script_utxo_data, _ = helper.json_open('tmp/', 'utxo.json')
        # print(script_utxo_data)
        # for utxo in script_utxo_data:
        #     print(utxo)
        #     # find the pkh that produces this.
        #     print(script_utxo_data[utxo]['datumhash'])
            # 
            



    def run_game(self, wallet_path: str):
        self.send_from_sc_to_sc(wallet_path)


    def abandon_game(self):
        pass

if __name__ == '__main__':
    import doctest
    doctest.testmod()
    
