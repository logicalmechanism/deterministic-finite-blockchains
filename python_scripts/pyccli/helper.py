import json
import os
import glob

def check_if_valid_json(value):
    """
    Test if a value is valid json.
    """
    try:
        json.loads(value)
        return True
    except json.decoder.JSONDecodeError:
        return False


def check_if_file_exists(path):
    return os.path.isfile(path)


def delete_contents(tmp):
    """
    Delete everything inside a folder called tmp.
    """
    files = glob.glob(tmp+'*')
    for f in files:
        os.remove(f)


def json_open(tmp, file_name):
    """
    Attempts to open a json file a tmp/file_name. Return the json data else
    return a empty dict.
    """
    if not isinstance(tmp, str):
        return {}, "TMP Folder Not String"
    if not isinstance(file_name, str):
        return {}, "File Name Not String"
    if 'json' not in file_name.split('.'):
        return {}, "File Is Not JSON."
    try:
        with open(tmp+file_name, "r") as read_content:
            data = json.load(read_content)
    except FileNotFoundError:
        return {}, "File Not Found."
    return data, ""


def whichnet(mainnet_flag):
    """
    Check if the flag is bool then proceed to determine which net to use. If
    the flag is not a boolean then just return the mainnet.
    """
    if isinstance(mainnet_flag, bool):
        if mainnet_flag is True:
            return ['--mainnet']
        else:
            return ['--testnet-magic', '1097911063']
    else:
        return ['--mainnet']


if __name__ == "__main__":
    print("Testing query.py")
