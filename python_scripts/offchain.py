import requests
# from typing import Tuple
###############################################################################

"""
A python class for building transactions required by a DFB.

A testnet or testnet blockfrost API key is required.
"""
class OffChain:

    
    # Data that needs to be stored.
    def __init__(self, api_key, script_address):
        self.currency      = {}
        self.utxo_addr_map = {}
        self.utxo_curr_map = {}
        self.addr_curr_map = {}
        self.api_key       = api_key
        self.headers       = { 'Project_id': api_key}
        self.address       = script_address
        self.minimum       = 3
        self.block_hash    = ""


    def get(self, endpoint: str) -> dict:
        """
        Return the json reponse from an endpoint.

        >>> OffChain("Wrong", "").get("nope")
        {}

        >>> OffChain("Wrong", "").get("https://cardano-testnet.blockfrost.io/api/v0/")
        {}
        """
        try:
            response = requests.get(endpoint, headers=self.headers).json()
            try:
                response['status_code']
                return {}
            except (KeyError, TypeError):
                return response
        except requests.exceptions.MissingSchema:
            return {}
    
    def find_latest_block(self) -> None:
        """
        Returns the latest block on blockfrost.

        >>> x = OffChain("", "")
        >>> x.find_latest_block()
        >>> bool(x.utxo_addr_map)
        False
        """
        url = 'https://cardano-testnet.blockfrost.io/api/v0/blocks/latest'
        try:
            self.block_hash = self.get(url)['hash']
        except KeyError:
            pass


    
    def find_everything(self) -> None:
        self.find_all_currency()
        self.find_all_utxos()
        self.find_all_addresses()
        self.find_latest_block() 
        for addr_tx_hash in self.utxo_addr_map:
            addr = self.utxo_addr_map[addr_tx_hash]
            for curr_tx_hash in self.utxo_curr_map:
                self.addr_curr_map[addr] = self.utxo_curr_map[curr_tx_hash]


    def find_all_addresses(self) -> None:
        """
        Return a dict mapping of tx hashes to input addresses.
        
        Assumes that the first address inside the tx input is the return address.

        >>> x = OffChain("", "")
        >>> x.find_all_addresses()
        >>> bool(x.utxo_addr_map)
        False
        """
        for tx_hash in self.utxo_curr_map:
            url = 'https://cardano-testnet.blockfrost.io/api/v0/txs/{}/utxos'.format(tx_hash)
            url_response = self.get(url)
            if bool(url_response) is True:
                self.utxo_addr_map[tx_hash] = url_response['inputs'][0]['address']
    
    
    def find_all_utxos(self) -> None:
        """
        Return a dict mapping tx hashes to amounts.

        >>> x = OffChain("", "")
        >>> x.find_all_utxos()
        >>> bool(x.utxo_curr_map)
        False
        """
        page = 1
        while True:
            url = 'https://cardano-testnet.blockfrost.io/api/v0/addresses/{}/utxos?page={}'.format(self.address, page)
            url_response = self.get(url)
            if bool(url_response) is False:
                break
            for obj in url_response:
                # print(obj['amount'])
                for index, token in enumerate(obj['amount']):
                    obj['amount'][index]['quantity'] = int(obj['amount'][index]['quantity'])
                self.utxo_curr_map[obj['tx_hash']] = obj['amount']
            page += 1


    def find_all_currency(self) -> None:
        """
        Return a dict of all the currency inside some address.
        
        >>> x = OffChain("", "")
        >>> x.find_all_currency()
        >>> bool(x.currency)
        False

        """
        url = 'https://cardano-testnet.blockfrost.io/api/v0/addresses/{}'.format(self.address)
        url_response = self.get(url)
        if bool(url_response) is True:
            amount = url_response['amount']
            for asset in amount:
                self.currency[asset['unit']] = int(asset['quantity'])


if __name__ == '__main__':
    import doctest
    doctest.testmod()