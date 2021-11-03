from dataclasses import dataclass

from core import AutoFunctor
from schemes import *


# Structures
class Peano(metaclass=AutoFunctor):
    pass


@dataclass
class Z(Peano):
    pass


@dataclass
class S(Peano):
    pred: Peano


# Algebras
def to_int_alg(fa):
    if isinstance(fa, Z): return 0
    elif isinstance(fa, S): return fa.pred + 1


def fib_alg(fh):
    if isinstance(fh, Z): return 1
    elif isinstance(fh.pred.fa, Z): return 1
    else: return fh.pred.a + fh.pred.fa.pred.a


def from_int_coalg(seed):
    if seed == 0: return Z()
    else: return S(seed - 1)


# Usage
def run():
    print([histo(fib_alg)(ana(from_int_coalg)(i)) for i in range(10)])


if __name__ == '__main__':
    run()
