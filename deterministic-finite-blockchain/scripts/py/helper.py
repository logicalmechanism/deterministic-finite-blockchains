import hashlib

def hashToNumber(string):
    """
    >>> hashToNumber("67b176705b46206614219f47a05aee7ae6a3edbe850bbbe214c536b989aea4d2")
    1317

    >>> hashToNumber(getHash("1"))
    1317
    """
    return reduction(sum_pair_list(string_to_list_in_pairs(string)))

def string_to_list_in_pairs(s):
    """
    >>> string_to_list_in_pairs("abcd")
    ['ab', 'bc', 'cd']
    """
    return [''.join(pair) for pair in zip(s[:-1], s[1:])]


def sum_pair_list(s):
    """
    >>> x = string_to_list_in_pairs("67b176705b46206614219f47a05aee7ae6a3edbe850bbbe214c536b989aea4d2")
    >>> sum_pair_list(x)
    82751641072219950866771297305292165320620762024983914138828800000
    """
    total = 1
    c = 0
    for value in s:
        if c % 2 == 0:
            total *= (int(value, 16) + 1)
        c += 1
    return total
        

def shuffle(array):
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

    >>> reduction(82751641072219950866771297305292165320620762024983914138828800000)
    1317
    """
    number = int(number)
    counter = 0
    while number != 1:
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
    Stringify ctr and sha3 512 hash then hexdigest.

    Force length to 64.

    >>> getHash("")
    'a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a'
    
    >>> getHash("hello, world")
    'bfb3959527d7a3f2f09def2f6915452d55a8f122df9e164d6f31c7fcf6093e14'

    >>> 
    """
    m = hashlib.sha3_256()
    string = str(ctr).encode('utf-8')
    m.update(bytes(string))
    ctr_hash = m.hexdigest()
    return ctr_hash[:48]


if "__main__" == __name__:
    import doctest
    doctest.testmod()