"""
The collection of recursion schemes this library provides.

By convention:
`run` refers to the executor applying the provides (co-)algebra(s).
`fa` refers to any functor (object with a `map` method).
`alg` refers to an algebra that returns any value when processing `fa`.
`a` refers to any value.
`coalg` refers to a co-algebra that returns `fa` when processing any value.
"""
from util import CoFree, Free, Pure, Insert


# Folds
def cata(alg):
    """
    Breaks down a data structure recursively.
    """
    def run(fa):
        return alg(fa.map(run))
    return run


def para(alg):
    """
    Extending a catamorphism by exposing the original data structure.
    """
    def run(fa):
        return alg(fa.map(run), fa)
    return run


def prothesi(alg):
    """
    Extending an catamorphism by exposing the future path.
    """
    def run(fa, todo=()):
        return alg(fa.map(lambda x: run(x, (fa,) + todo)), todo)
    return run


def histo(alg):
    """
    Extending a catamorphism by exposing the processed tree.
    """
    def run(fa):
        return cata(lambda x: CoFree(alg(x), x))(fa).a
    return run


# Unfolds
def ana(coalg):
    """
    Dual to a catamorphism, ana builds a data structure over which you can map.
    """
    def run(a):
        return coalg(a).map(run)
    return run


def apo(coalg):
    """
    Extending an anamorphism with the ability to halt.
    In this version, a boolean is paired with the value that indicates halting.
    """
    def run(a):
        stop, fa = coalg(a)
        return fa if stop else fa.map(run)
    return run


def prop_apo(coalg, pred):
    """
    Like apo, but using a separate predicate function to determine stopping.
    """
    def run(a):
        fa = coalg(a)
        return fa if pred(fa) else fa.map(run)
    return run


def option_apo(coalg):
    """
    Like apo, but halting when the co-algebra function returns None.
    """
    def run(a):
        optional_fa = coalg(a)
        return a if optional_fa is None else optional_fa.map(run)
    return run


def ichno(coalg):
    """
    Dual to prothesi, extending an anamorphism by exposing the current trace.
    """
    def run(a, trace=()):
        fa = coalg(a, trace)
        return fa.map(lambda x: run(x, (fa,) + trace))
    return run


def futu(coalg):
    """
    Unfolding like an anamorphism, but with multiple steps at a time with the use of a tree.
    The co-algebra skips the bulk of the `Free` tree, only starting again at the `Pure` leaves.
    """
    def traverse_free(free):
        if isinstance(free, Pure): return free.a.map(run)
        elif isinstance(free, Bind): return free.fa.map(traverse_free)

    def run(a):
        return traverse_free(coalg(a))
    return run


def insert_futu(coalg):
    """
    Unfolding like an anamorphism, but with multiple steps at a time with the use of Insert.
    The co-algebra is not run on elements wrapped in Insert.
    """
    def traverse_insert(fa):
        return fa.v.map(traverse_insert) if isinstance(fa, Insert) else fa.v.map(run)

    def run(a):
        return traverse_insert(coalg(a))
    return run


# Combinations
def hylo(alg, coalg):
    """
    Simultaneously unfolding with an anamorphism and folding with catamorphism.
    """
    def run(a):
        return alg(coalg(a).map(run))
    return run  # equivalent to lambda a: cata(alg)(ana(coalg)(a))


def chrono(alg, coalg):
    """
    Performing a histomorphism right after a futumorphism.
    """
    return lambda a: histo(alg)(futu(coalg)(a))
