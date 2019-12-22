#!/usr/bin/env python

MIN = 264360
MAX = 746325


def valid(pw):
    as_str = '%s' % pw
    has_double = False

    for i in range(len(as_str) - 1):
        if int(as_str[i]) > int(as_str[i + 1]):
            return False

    for i in range(len(as_str) - 1):
        c0 = as_str[i-1] if i > 0 else None
        c1 = as_str[i]
        c2 = as_str[i+1]
        c3 = as_str[i+2] if i < len(as_str) - 2 else None

        if c1 == c2:
            if c1 != c0 and c1 != c3:
                has_double = True

    return has_double


def solve(minimum, maximum):
    return len([x for x in range(minimum, maximum + 1) if valid(x)])


if __name__ == '__main__':
    print(solve(MIN, MAX))
