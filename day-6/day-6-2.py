#!/usr/bin/env python


def common_path(xpath, ypath):
    for i in range(len(xpath)):
        if i >= len(ypath):
            return -1

        if xpath[i] != ypath[i]:
            return (len(xpath) - i) + (len(ypath) - i)


def find_path(target, orb_map):
    def helper(m, target, path, k):
        if orb_map.get(k):
            for c in orb_map.get(k):
                if c == target:
                    return path
                else:
                    newpath = path[:]
                    newpath.insert(0, c)
                    candidate = helper(m, target, newpath, c)

                    if candidate:
                        return candidate
        else:
            return []

    return helper(orb_map, target, [], 'COM')


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
    parsed = parse(ipt)
    you_path = list(reversed(find_path('YOU', parsed)))
    san_path = list(reversed(find_path('SAN', parsed)))

    return common_path(you_path, san_path)


if __name__ == '__main__':
    with open('day-6/input.txt') as inputfile:
        print(solve(inputfile.readlines()))
