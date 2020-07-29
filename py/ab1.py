"""
Usage:
  ab1.py [options] <port>
  ab1.py act <x> <y>

Options:
  -h --help                show this help message and exit
  --version                show version and exit
  -n, --number N           use N as a number
  -t, --timeout TIMEOUT    set timeout TIMEOUT seconds
  --apply                  apply changes to database
  -q                       operate in quiet mode
"""
from docopt import docopt


if __name__ == '__main__':
  arguments = docopt(__doc__, version='1.0.0rc2')
  print(arguments)
