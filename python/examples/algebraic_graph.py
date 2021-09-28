from dataclasses import dataclass
from typing import Any

from util import AutoFunctor
from schemes import *


# Structures
class AGraph(metaclass=AutoFunctor):
    def __add__(self, other):
        return Overlay(self, other)

    def __mul__(self, other):
        return Connect(self, other)


@dataclass
class Empty(AGraph):
    pass


@dataclass
class Vertex(AGraph):
    data: Any


@dataclass
class Overlay(AGraph):
    x: AGraph
    y: AGraph


@dataclass
class Connect(AGraph):
    x: AGraph
    y: AGraph


# Algebras
def size_alg(fa):
    if isinstance(fa, Empty): return 0
    elif isinstance(fa, Vertex): return 1
    elif isinstance(fa, Overlay): return fa.x + fa.y
    elif isinstance(fa, Connect): return fa.x + fa.y


def node_map(f):
    def node_map_alg(fa):
        if isinstance(fa, Empty): return fa
        elif isinstance(fa, Vertex): return f(fa)
        elif isinstance(fa, Overlay): return fa
        elif isinstance(fa, Connect): return fa
    return node_map_alg


def vertexset_alg(fa):
    if isinstance(fa, Empty): return {}
    elif isinstance(fa, Vertex): return {fa.data}
    elif isinstance(fa, Overlay): return fa.x | fa.y
    elif isinstance(fa, Connect): return fa.x | fa.y


def simplify_alg(fa):
    ...


# Usage
def run():
    g = Vertex('x')*(Vertex('y') + Vertex('z'))
    print(cata(size_alg)(g))

    values = {'x': 1, 'y': 2, 'z': 3}
    ig = cata(node_map(lambda x: Vertex(values[x.data])))(g)
    print(sum(cata(vertexset_alg)(ig)))

    def graph_induce(p): return node_map(lambda x: x if p(x) else Empty())
    print(cata(graph_induce(lambda v: v.data != 'x'))(g))


if __name__ == '__main__':
    run()
