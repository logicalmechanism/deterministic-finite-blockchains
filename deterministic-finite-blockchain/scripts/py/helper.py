import hashlib

def hashToNumber(string):
    """
    >>> hashToNumber([])
    1
    
    >>> x = getHash("")
    >>> hashToNumber(x)
    2739796737785856

    >>> x = getHash("some string")
    >>> hashToNumber(x)
    1594568333280

    >>> x = getHash("4c72e63e5a6a66e04411b3151c939518a86895a5fcbdaddeb53d86ef12e4eb64c4fe5a66ff8c1bdd4a88f7b9a171172cef99e769161eb5e8d5c902a38471b8ac")
    >>> hashToNumber(x)
    9948443392800
    """
    # return reduction(sum_pair_list(string_to_list_in_pairs(string)))
    return sum_pair_list(string_to_list_in_pairs(string)[:14])

def string_to_list_in_pairs(s):
    """
    >>> string_to_list_in_pairs("abcd")
    ['ab', 'bc', 'cd']

    >>> string_to_list_in_pairs("")
    []

    """
    return [''.join(pair) for pair in zip(s[:-1], s[1:])]


def sum_pair_list(s:list) -> int:
    """
    >>> sum_pair_list(['af'])
    176

    >>> sum_pair_list([])
    1

    >>> x = getHash("")
    >>> y = string_to_list_in_pairs(x)
    >>> sum_pair_list(y)
    5945612572284791548098908064394044345263707520646381729396817920000
    """
    total = 1
    c = 0
    for value in s:
        if c % 2 == 0:
            # print(((b10(value)) + 1))
            total *= ((b10(value)) + 1)
        c += 1
    # print(c)
    return total
        

def shuffle(array: list) -> list:
    """
    Shuffle an array with its own hash.

    >>> shuffle([1,2,3,4])
    [1, 4, 3, 2]

    >>> shuffle([1,2,3,4,5,6,7,8,9])
    [9, 6, 8, 5, 4, 1, 2, 3, 7]
    """
    N = len(array)
    mix = bq(b10(getHash(array)), N)[:N]
    play = [i for i in range(N)]
    for x,y in zip(play, mix):
        old = array[x]
        array[x] = array[y]
        array[y] = old
    return array


def reduction(number: int) -> int:
    """
    Returns the trajectory length of classic 3n+1.

    >>> reduction(142)
    103

    >>> reduction('132')
    28

    """
    number = int(number)
    counter = 0
    while number != 1 and number > 0:
        if number % 2 == 0:
            number = number // 2
        else:
            number = 3*number + 1
        counter += 1
    return counter


def pairs(x: list, y:list) -> list:
    """
    Zips two lists together as a list of tuple pairs.

    >>> pairs([1,2,3],['a','b','c'])
    [(1, 'a'), (2, 'b'), (3, 'c')]

    >>> pairs([],[])
    []
    """
    z = zip(x,y)
    return [(i,j) for i,j in z if i != j]


def b10(obj:str) -> int:
    """
    Converts some hash into base 10.

    >>> b10('0')
    0

    >>> b10(0)
    0

    >>> b10("af")
    175

    >>> b10(getHash("testing function"))
    51907786325732841100955856297744881885868012143421440929942953657044720863612
    """
    return int(str(obj), 16)


def bq(number: int, base: int) -> list:
    """
    Convert base 10 into base base.

    >>> bq(23, 3)
    [2, 1, 2]

    >>> bq(0, 3)
    [0]
    """
    if number == 0:
        return [0]
    digits = []
    while number:
        digits.append(int(number % base))
        number //= base
    return digits[::-1]


def getHash(ctr: str) -> str:
    """
    Stringify ctr and sha3_256 hash then hexdigest.

    >>> getHash("")
    'a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a'
    
    >>> getHash("hello, world")
    'bfb3959527d7a3f2f09def2f6915452d55a8f122df9e164d6f31c7fcf6093e14'


    >>> getHash("4c72e63e5a6a66e04411b3151c939518a86895a5fcbdaddeb53d86ef12e4eb64c4fe5a66ff8c1bdd4a88f7b9a171172cef99e769161eb5e8d5c902a38471b8ac")
    '008a77ec7a5cdb04c7fa00d76b91e7471d7ab9ac504370caa9a1a027842b19a2'
    """
    m = hashlib.sha3_256()
    string = str(ctr).encode('utf-8')
    m.update(bytes(string))
    ctr_hash = m.hexdigest()
    return ctr_hash


if "__main__" == __name__:
    import doctest
    doctest.testmod()