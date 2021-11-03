from dataclasses import dataclass
from typing import Any

from core import AutoFunctor
from schemes import *


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


def mergesort_coalg(a):
    if not a: return Empty()
    elif len(a) == 1: return Leaf(a[0])
    else:
        middle = len(a)//2
        return Branch(a[:middle], a[middle:])


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
