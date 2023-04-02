import sys
import os


def main():
    try:
        cli.dbtree()
        sys.exit()
    except SystemExit:
        os._exit(1)


if __name__ == '__main__':
    main()
