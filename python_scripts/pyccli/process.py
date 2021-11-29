import subprocess


def output(func):
    """
    Read the output of a function from stout and returns as decoded utf-8 and exit code.
    """
    try:
        process_output = subprocess.check_output(func, encoding='utf-8').strip()
        exit_code      = 0
    except subprocess.CalledProcessError:
        process_output = ''
        exit_code      = 1
    return process_output, exit_code



if __name__ == "__main__":
    print("Testing process.py")
    print("\nThis should pass...\n")
    func_pass = [
        'cardano-cli',
        '--version'
    ]
    print(' '.join(func_pass))
    p, e = output(func_pass)
    print("\nOUTPUT: {}".format(p))
    print("EXIT CODE: {}".format(e))
    print("PASS: {}".format(e == 0))

    print("\nThis should fail...\n")
    func_fail = [
        'cardano-cli',
        'verssion'
    ]
    print(' '.join(func_fail))
    p, e = output(func_fail)
    print("\nOUTPUT: {}".format(p))
    print("EXIT CODE: {}".format(e))
    print("PASS: {}".format(e == 0))
