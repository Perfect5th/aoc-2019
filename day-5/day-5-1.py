#!/usr/bin/env python
import os


def noop(*args):
    pass


def add(val1, val2, *args):
    return val1 + val2


def mult(val1, val2, *args):
    return val1 * val2


class IntCodeComputer:
    OPS = {'ADD': 1, 'MULT': 2, 'IN': 3, 'OUT': 4, 'HALT': 99}
    IO_OPS = [OPS['IN'], OPS['OUT']]

    def __init__(self, tape, inputstream, outputstream):
        self.position = 0
        self.tape = tape
        self.funcs = [noop, add, mult, self.input, self.output]
        self.halted = False

        self.inputstream = inputstream
        self.outputstream = outputstream

    def run(self):
        while not self.halted:
            self.fetch()
            self.decode()
            self.execute()
            self.writeback()

    def fetch(self):
        self.opcode = self.tape[self.position]
        self.op = self.opcode % 100
        self.param_1_mode = self.opcode // 100 % 10
        self.param_2_mode = self.opcode // 1000 % 10
        self.param_3_mode = self.opcode // 10000 % 10

    def decode(self):
        self.val_1 = self.get_value(1, self.param_1_mode)
        self.val_2 = self.get_value(2, self.param_2_mode)
        self.val_3 = self.get_value(3, 1)

        self.func = self.get_func()
        self.val_p = self.get_val_p()

    def execute(self):
        self.val_e = self.func(self.val_1, self.val_2, self.val_3)

    def writeback(self):
        if self.op < 3:
            self.tape[self.val_3] = self.val_e

        if self.op == IntCodeComputer.OPS['IN']:
            self.tape[self.val_1] = self.read_val

        if self.op == IntCodeComputer.OPS['HALT']:
            self.halted = True

        self.position += self.val_p

    def get_value(self, pos, mode):
        if self.op == IntCodeComputer.OPS['HALT']:
            return None

        if self.op in IntCodeComputer.IO_OPS and pos > 1:
            return None

        if mode == 1 or self.op == IntCodeComputer.OPS['IN']:
            return self.tape[self.position + pos]

        return self.tape[self.tape[self.position + pos]]

    def get_val_p(self):
        if self.op in IntCodeComputer.IO_OPS:
            return 2

        if self.op == IntCodeComputer.OPS['HALT']:
            return 1

        return 4

    def get_func(self):
        if self.op >= len(self.funcs):
            return noop

        return self.funcs[self.op]

    def input(self, val1, val2, *args):
        self.read_val = self.get_input()

    def output(self, val1, val2, *args):
        self.write_output(val1)

    def get_input(self):
        return int(self.inputstream())

    def write_output(self, val):
        self.outputstream(val)


def solve(ipt):
    comp = IntCodeComputer(ipt, input, print)
    comp.run()


if __name__ == '__main__':
    with open(os.path.join(os.path.dirname(__file__), 'input.txt')) as iptfile:
        solve([int(x) for x in iptfile.readlines()[0][:-1].split(',')])
