import hashlib
from functools import reduce
"""
"""
class DFB:


    # Data that needs to be stored.
    def __init__(self, wallets: dict, currency: dict, starting_hash: str) -> None:
        self.wallets      = self.initializeWallets(wallets)
        self.reserves     = currency
        self.N            = len(wallets)
        self.tokens       = len(currency) - 1
        self.halving      = 100
        self.mining_rate  = 50
        self.mining_pool  = (len(self.bq(self.b10(self.getHash(self.N)), self.N))+2)*self.mining_rate*self.halving*2
        self.block_hash   = starting_hash
        self.block_number = 1
        self.extend       = True


    def increment_block_number(self) -> None:
        self.block_number += 1
        self.block_hash = self.getHash(self.block_hash)


    def initializeWallets(self, wallet: dict) -> dict:
        """
        Return an appended dictionary of all the wallets. 
        This accounts for the mined token.
        
        >>> DFB({}, {}, "").initializeWallets({'addr': []})
        {'addr': [{'unit': 'synthetic_asset', 'quantity': 0}]}
        """
        synthetic_asset = {
            'unit': 'synthetic_asset',
            'quantity': 0
        }
        for addr in wallet:
            wallet[addr].append(synthetic_asset)
        return wallet

    
    def mining(self):
        """
        The Mining sequence.
        """
        mining_list = self.bq(self.b10(self.block_hash), self.N)
        list_of_wallets_addr = list(self.wallets)
        # It reduces to one
        if self.mining_rate <= 1:
            self.mining_rate = 1
        # half the mining rate ever halving
        if self.block_number % self.halving == 0 and self.mining_rate > 1:
            self.mining_rate = self.mining_rate // 2
        # mine the pool
        for miner in mining_list:
            addr = list_of_wallets_addr[miner]
            if self.mining_pool <= 0:
                self.mining_pool = 0
                self.extend = False
                break
            for index, asset in enumerate(self.wallets[addr]):
                if asset['unit'] == 'synthetic_asset':
                    self.wallets[addr][index]['quantity'] += self.mining_rate
                    self.mining_pool -= self.mining_rate
    

    def trading(self):
        """
        The Trading Sequence.
        """
        phase_1_number = self.b10(self.block_hash)
        phase_2_number = self.b10(self.getHash(phase_1_number))
        phase_1_tokens = self.bq(phase_1_number, self.tokens)
        phase_2_tokens = self.bq(phase_2_number, self.tokens)
        phase_1_list = self.bq(phase_1_number, self.N)
        phase_2_list = self.bq(phase_2_number, self.N)
        list_of_wallets_addr = list(self.wallets)
        list_of_policy_ids = list(self.reserves)
        
        # This is what we need
        amount = self.number_reduction(phase_2_number)
        trading_pairs = self.pairs(phase_1_list, phase_2_list)
        token_pairs = self.pairs(phase_1_tokens, phase_2_tokens)
        
        # Now lets trade
        successful_trading_flag = False
        for trade, token in zip(trading_pairs, token_pairs):
            A_addr = list_of_wallets_addr[trade[0]]
            B_addr = list_of_wallets_addr[trade[1]]
            A = self.wallets[A_addr] # assets for wallet A
            B = self.wallets[B_addr] # assets for wallet B
            token_1_pid = list_of_policy_ids[token[0]]
            token_2_pid = list_of_policy_ids[token[1]]
            # amount token 1 
            # delta token 2
            delta = self.calculate_delta_amount(token_1_pid, token_2_pid, amount)
            if delta > 0:
                for tokens in A:
                    if tokens['unit'] == token_1_pid:
                        if tokens['quantity']-amount > 0:
                            successful_trading_flag = True
            if successful_trading_flag is True:
                for i in range(len(self.wallets[A_addr])):
                    if self.wallets[A_addr][i]['unit'] == token_1_pid:
                        self.wallets[A_addr][i]['quantity'] -= amount
                    if self.wallets[A_addr][i]['unit'] == token_2_pid:
                        self.wallets[A_addr][i]['quantity'] += delta
                    else:
                        new_token = {
                            'unit': token_2_pid,
                            'quantity': delta
                        }
                        self.wallets[A_addr].append(new_token)
                for i in range(len(self.wallets[B_addr])):
                    if self.wallets[B_addr][i]['unit'] == token_2_pid:
                        self.wallets[B_addr][i]['quantity'] -= delta
                    if self.wallets[B_addr][i]['unit'] == token_1_pid:
                        self.wallets[B_addr][i]['quantity'] += amount
                    else:
                        new_token = {
                            'unit': token_1_pid,
                            'quantity': amount
                        }
                        self.wallets[B_addr].append(new_token)
        self.extend = successful_trading_flag


    def calculate_delta_amount(self, ith_token_pid, jth_token_pid, ith_amount):
        """
        Return the delta amount from the constant product market
        """
        k = 1
        for currency in self.reserves:
            k *= self.reserves[currency]
        c = 1
        for currency in self.reserves:
            if currency not in [ith_token_pid, jth_token_pid]:
                c *= self.reserves[currency]
        R1 = self.reserves[ith_token_pid]
        R2 = self.reserves[ith_token_pid]
        return int(R2- (k)*(1/(c*(R1-ith_amount))))+1 # ceiling
        

    def getHash(self, string:str) -> str:
        """
        Stringify the string and sha3 512 hash then hexdigest.

        >>> DFB({}, {}, "").getHash('Hello, World!')
        '38e05c33d7b067127f217d8c856e554fcff09c9320b8a5979ce2ff5d95dd27ba35d1fba50c562dfd1d6cc48bc9c5baa4390894418cc942d968f97bcb659419ed'

        >>> DFB({}, {}, "").getHash(1)
        'ca2c70bc13298c5109ee0cb342d014906e6365249005fd4beee6f01aee44edb531231e98b50bf6810de6cf687882b09320fdd5f6375d1f2debd966fbf8d03efa'
        """
        string = str(string)
        m = hashlib.sha3_512()
        string = str(string).encode('utf-8')
        m.update(bytes(string))
        return m.hexdigest()
    

    def b10(self, obj: str):
        """
        Returns the integer value of stringed number in base 16.

        >>> DFB({}, {}, "").b10('test')
        0

        >>> DFB({}, {}, "").b10('ca')
        202

        >>> DFB({}, {}, "").b10(1)
        1
        """
        try:
            obj = str(obj)
            target = int(obj, 16)
        except ValueError:
            return 0 # Align with int function
        return target
    
    
    def bq(self, number: int, base: int) -> list:
        """
        Returns a list of a number in base 10 in base b.

        >>> DFB({}, {}, "").bq(5, 10)
        [5]

        >>> DFB({}, {}, "").bq(11, 2)
        [1, 0, 1, 1]

        >>> DFB({}, {}, "").bq(11, -2)
        [0]

        >>> DFB({}, {}, "").bq(-11, 2)
        [0]
        """
        if number <= 0:
            return [0]
        if base <= 1:
            return [0]
        digits = []
        while number:
            digits.append(int(number % base))
            number //= base
        return digits[::-1]
    

    def number_reduction(self, number: int) -> int:
        """
        Returns the length of a trajectory in the classic 3n+1 problem.

        >>> DFB({}, {}, "").number_reduction(10)
        6

        >>> DFB({}, {}, "").number_reduction('a')
        0
        """
        try:
            number = int(number)
        except (TypeError, ValueError):
            return 0 # Return zero for anything else
        counter = 0
        while number != 1:
            if number % 2 == 0:
                number = number // 2
            else:
                number = 3*number + 1
            counter += 1
        return counter

    
    def pairs(self, x, y):
        """
        Return a list of the tuple pairs of two lists
        
        >>> DFB({}, {}, "").pairs([1,2,3], [4,5,6])
        [(1, 4), (2, 5), (3, 6)]

        >>> DFB({}, {}, "").pairs([1,2], [4,5,6])
        [(1, 4), (2, 5)]
        """
        z = zip(x,y) # Shortest list wins the length battle
        return [(i,j) for i,j in z if i != j]


if __name__ == '__main__':
    import doctest
    doctest.testmod()