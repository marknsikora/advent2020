#!/usr/bin/env python3

import argparse
import itertools

def number_game(ages):
    n = ages[-1]
    start = len(ages)
    ages = {age: i for i, age in enumerate(ages[:-1], 1)}

    for i in itertools.count(start):
        prev = n

        try:
            n = i - ages[n]
        except KeyError:
            n = 0

        ages[prev] = i

        yield i + 1, n

def main():
    parser = argparse.ArgumentParser()

    parser.add_argument('file')

    args = parser.parse_args()

    with open(args.file) as fp:
        text = fp.read()
        data = [int(i) for i in text.split(',')]

    ans = number_game(data)

    for i, n in ans:
        if i == 2020:
            print(n)
        elif i == 30000000:
            print(n)
            break

if __name__ == '__main__':
    main()
