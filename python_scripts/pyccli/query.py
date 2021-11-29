import pyccli.process as process
import pyccli.helper as helper

def protocol_parameters(tmp='', file_name='protocol_parameters.json', mainnet_flag=True):
    """
    Query the protocol parameters and save to the tmp folder.

    Requires node socket to be on path.
    """
    func = [
        'cardano-cli',
        'query',
        'protocol-parameters',
        '--out-file',
        tmp+file_name
    ]
    func += helper.whichnet(mainnet_flag)
    return process.output(func)


def tip(tmp='', file_name='tip.json', mainnet_flag=True):
    """
    Query the tip of the blockchain then save to a file.
    """
    func = [
        'cardano-cli',
        'query',
        'tip',
        '--out-file',
        tmp+file_name
    ]
    func += helper.whichnet(mainnet_flag)
    return process.output(func)


def utxo(address, tmp='', file_name='utxo.json', mainnet_flag=True):
    """
    Query the utxo from the wallet address and save to the tmp folder.
    """

    func = [
        'cardano-cli',
        'query',
        'utxo',
        '--address',
        address,
        '--out-file',
        tmp+file_name
    ]
    func += helper.whichnet(mainnet_flag)
    return process.output(func)


if __name__ == "__main__":
    print("Testing query.py")
    
    print("\nprotocol_parameters\n")
    p, e = protocol_parameters('tmp/')
    print("PASS: ", e == 0)
    
    print("\ntip\n")
    p, e = tip('tmp/')
    print("PASS: ", e == 0)
    
    print('\nUTxO\n')
    address = "addr1qyxds227n9sr58mykudwztu6nve260vsf47p6vskk3cdmnajl7727rk6glhz9e4sqnp7xqew2m5u44qpzjh2e3dtv92qjsryey"
    p,e = utxo(address, 'tmp/')
    print("PASS: ", e == 0)
    