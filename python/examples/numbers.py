from dataclasses import dataclass, field
from typing import Any

from util import AutoFunctor
from schemes import ana, cata, histo


# Structures
class Peano(metaclass=AutoFunctor):
    pass


@dataclass
class Z(Peano):
    pass


@dataclass
class S(Peano):
    prev: Peano


# Algebras
def to_int_alg(fa):
    if isinstance(fa, Z): return 0
    elif isinstance(fa, S): return fa.prev + 1


def fib_alg(fh):
    if isinstance(fh, Z): return 1
    elif isinstance(fh.prev.fa, Z): return 1
    else: return fh.prev.a + fh.prev.fa.prev.a


def from_int_coalg(seed):
    if seed == 0: return Z()
    else: return S(seed - 1)


# Usage
def run():
    print([histo(fib_alg)(ana(from_int_coalg)(i)) for i in range(10)])


if __name__ == '__main__':
    run()
