from dataclasses import dataclass
from typing import Any

from util import AutoFunctor
from schemes import cata, hylo, para, apo


# Structures
class BinaryTree(metaclass=AutoFunctor):
    pass


@dataclass
class Empty(BinaryTree):
    pass


@dataclass
class Leaf(BinaryTree):
    x: Any


@dataclass
class Branch(BinaryTree):
    l: BinaryTree
    r: BinaryTree


# Algebras
def string_alg(fa):
    if isinstance(fa, Branch): return f"({fa.l}, {fa.r})"
    elif isinstance(fa, Leaf): return f"{fa.x}"
    elif isinstance(fa, Empty): return "."


def mergesort_coalg(seed):
    if not seed: return Empty()
    elif len(seed) == 1: return Leaf(seed[0])
    else:
        middle = len(seed)//2
        return Branch(seed[:middle], seed[middle:])


def merge(l, r):
    if not r: return l
    if not l: return r

    x, *xs = l; y, *ys = r

    return [x, *merge(xs, r)] if x < y else [y, *merge(l, ys)]


def mergesort_alg(fa):
    if isinstance(fa, Empty): return []
    elif isinstance(fa, Leaf): return [fa.x]
    elif isinstance(fa, Branch): return merge(fa.l, fa.r)


# Usage
def run():
    print(hylo(mergesort_alg, mergesort_coalg)("ADEBC"))

    print(cata(string_alg)(Branch(Leaf(1), Branch(Branch(Leaf(2), Empty()), Leaf(2)))))


if __name__ == '__main__':
    run()
