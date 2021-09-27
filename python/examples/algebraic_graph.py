from dataclasses import dataclass
from typing import Any

from util import AutoFunctor
from schemes import cata, hylo, para, apo


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


def simplify_alg(fa, ofa):
    if isinstance(fa, Empty): return Empty()
    elif isinstance(fa, Vertex): return {fa.data}
    elif isinstance(fa, Overlay): return fa.x | fa.y
    elif isinstance(fa, Connect): return fa.x | fa.y


# Usage
def run():
    g = Vertex('x')*(Vertex('y') + Vertex('z'))
    print(cata(size_alg)(g))
    print(cata(node_map(lambda x: x.data*2))(g))

    def graph_induce(p):
        return node_map(lambda x: x if p(x) else Empty())

    print(cata(graph_induce(lambda v: v.data != 'x'))(g))
    print(cata(vertexset_alg)(g))


if __name__ == '__main__':
    run()
