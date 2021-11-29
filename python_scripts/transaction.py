import subprocess
import pyccli as cli
import os
from sys import exit
"""
"""
class Tx:

    # Data that needs to be stored.
    def __init__(self, mainnet_flag: bool) -> None:
        self.outgoing = []
        self.incoming = []
        self.mainnet  = mainnet_flag
    
    def send_from_wallet_into_sc(self, wallet_path) -> bool:
        """
        Send all the assets from a wallet into the smart contract.
        Return a bool upon completion.

        >>> Tx(False).send_from_wallet_into_sc('')
        False

        >>> Tx(False).send_from_wallet_into_sc('../wallets/')
        True
        """
        if os.path.isdir(wallet_path) is False:
            return False
        directory_list = list()
        for root, dirs, files in os.walk(wallet_path, topdown=False):
            for name in dirs:
                directory_list.append(os.path.join(root, name))
        print(directory_list)
        return True


    
    def start_game(self):
        # design function to spend a single wallet into the sc
        # loop the function for all the wallets in the folder
        pass

    def run_game(self):
        pass

    def abandon_game(self):
        pass

if __name__ == '__main__':
    import doctest
    doctest.testmod()
    
