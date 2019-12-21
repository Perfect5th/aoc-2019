#!/usr/bin/env python

INPUT = [
    1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 10, 1, 19, 1, 6, 19, 23,
    1, 23, 13, 27, 2, 6, 27, 31, 1, 5, 31, 35, 2, 10, 35, 39, 1, 6, 39, 43, 1,
    13, 43, 47, 2, 47, 6, 51, 1, 51, 5, 55, 1, 55, 6, 59, 2, 59, 10, 63, 1, 63,
    6, 67, 2, 67, 10, 71, 1, 71, 9, 75, 2, 75, 10, 79, 1, 79, 5, 83, 2, 10, 83,
    87, 1, 87, 6, 91, 2, 9, 91, 95, 1, 95, 5, 99, 1, 5, 99, 103, 1, 103, 10,
    107, 1, 9, 107, 111, 1, 6, 111, 115, 1, 115, 5, 119, 1, 10, 119, 123, 2, 6,
    123, 127, 2, 127, 6, 131, 1, 131, 2, 135, 1, 10, 135, 0, 99, 2, 0, 14, 0]

TARGET = 19690720


class Instruction:
    def __init__(self, tape, pos):
        self.tape = tape
        self.op = tape[pos]
        self.arg1 = tape[pos + 1]
        self.arg2 = tape[pos + 2]
        self.out = tape[pos + 3]

    def exec(self):
        if self.op == 1:
            self.tape[self.out] = self.tape[self.arg1] + self.tape[self.arg2]
            return True
        elif self.op == 2:
            self.tape[self.out] = self.tape[self.arg1] * self.tape[self.arg2]
            return True

        return False


def run(tape):
    for i in range(0, len(tape), 4):
        inst = Instruction(tape, i)

        if not inst.exec():
            break

    return tape[0]


def run_loop(tape):
    curr_tape = tape[:]

    for i in range(99):
        for j in range(99):
            curr_tape[1] = i
            curr_tape[2] = j

            if run(curr_tape) == TARGET:
                return i, j

            curr_tape = tape[:]


if __name__ == '__main__':
    noun, verb = run_loop(INPUT)

    print(100 * noun + verb)
