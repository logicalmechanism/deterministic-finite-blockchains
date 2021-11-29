import pyccli.process as process
import pyccli.helper as helper


def key_hash(verification_key_path):
    func = [
        'cardano-cli', 
        'address', 
        'key-hash', 
        '--payment-verification-key-file', 
        verification_key_path
    ]
    return process.output(func)


def build(file_path, file_name, script_flag=False, mainnet_flag=True):
    """
    Builds a Shelly payment address.
    """
    if script_flag is True:
        type_of_file = [
            '--payment-script-file',
            file_path+file_name,
        ]
    else:
        type_of_file = [
            '--payment-verification-key-file',
            file_path+file_name
        ]
    func = [
        'cardano-cli',
        'address',
        'build',
    ]
    func += type_of_file
    func += helper.whichnet(mainnet_flag)
    return process.output(func)


def info(address):
    func = [
        'cardano-cli', 
        'address',
        'info', 
        '--address', 
        address
    ]
    return process.output(func)


if __name__ == "__main__":
    print("Testing address.py")

    print('\nGet the public key hash\n')
    vkey_path = 'tmp/payment.vkey'
    p, e = key_hash(vkey_path)
    print(p)
    print("PASS: ", e == 0)
    print('\nGet the address from vkey\n')
    p, e = build('tmp/', 'payment.vkey')
    print(p)
    print("PASS: ", e == 0)
    print('\nGet the address from plutus script\n')
    p, e = build('tmp/', 'script.plutus', True)
    print(p)
    print("PASS: ", e == 0)


    print('\nGet the address information\n')
    address = "addr1qyxds227n9sr58mykudwztu6nve260vsf47p6vskk3cdmnajl7727rk6glhz9e4sqnp7xqew2m5u44qpzjh2e3dtv92qjsryey"
    p, e = info(address)
    print(p)
    print("PASS: ", e == 0)



