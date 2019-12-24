#!/usr/bin/env python


def checksum(orb_map):
    def helper(m, depth, k):
        if m.get(k):
            return depth + sum([helper(m, depth + 1, cs) for cs in m.get(k)])
        else:
            return depth

    return helper(orb_map, 0, 'COM')


def add_to_map(orb, orb_map):
    parent, child = orb[:-1].split(')')

    if orb_map.get(parent):
        orb_map[parent].append(child)
    else:
        orb_map[parent] = [child]


def parse(ipt):
    orb_map = {}

    for i in ipt:
        add_to_map(i, orb_map)

    return orb_map


def solve(ipt):
    return checksum(parse(ipt))


if __name__ == '__main__':
    with open('day-6/input.txt') as inputfile:
        print(solve(inputfile.readlines()))
