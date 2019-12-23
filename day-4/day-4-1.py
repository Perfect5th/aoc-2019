#!/usr/bin/env python

MIN = 264360
MAX = 746325


def valid(pw):
    as_str = '%s' % pw
    has_double = False

    for i in range(len(as_str) - 1):
        if int(as_str[i]) > int(as_str[i + 1]):
            return False

        if as_str[i] == as_str[i+1]:
            has_double = True

    return has_double


def solve(minimum, maximum):
    return len([x for x in range(minimum, maximum + 1) if valid(x)])


if __name__ == '__main__':
    print(solve(MIN, MAX))
