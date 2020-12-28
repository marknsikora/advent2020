#!/usr/bin/env python3

import argparse
import itertools
import re


def crab_combat(numbers, rounds):
    first = None
    prev = None

    sequence = {}

    for n in numbers:
        if first is None:
            first = n
            lowest = n
            highest = n
        else:
            lowest = min(lowest, n)
            highest = max(highest, n)

        if prev:
            sequence[prev] = n

        prev = n

    sequence[n] = first

    current = first
    for _ in range(rounds):
        a = sequence[current]
        b = sequence[a]
        c = sequence[b]

        d = sequence[c]

        sequence[current] = d

        i = current - 1
        while True:
            if i in [a,b,c]:
                i -= 1
                continue

            try:
                sequence[i]
            except KeyError:
                pass
            else:
                break

            if i <= lowest:
                i = highest
            else:
                i -= 1

        j = sequence[i]

        sequence[c] = j
        sequence[i] = a

        current = sequence[current]

    return sequence


def answer_string(sequence):
    nums = []
    i = 1

    while True:
        i = sequence[i]

        if i == 1:
            break

        nums.append(i)

    return ''.join([str(n) for n in nums])


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('input')
    args = parser.parse_args()

    with open(args.input) as fp:
        for line in fp:
            digits = re.findall(r'\d', line)

    numbers = [int(digit) for digit in digits]

    sequence = crab_combat(numbers, 100)
    print(answer_string(sequence))

    m = max(numbers)
    rest = range(m+1, 1_000_000+1)

    sequence = crab_combat(itertools.chain(numbers, rest), 10_000_000)

    a = sequence[1]
    b = sequence[a]

    print(a * b)


if __name__ == '__main__':
    main()
