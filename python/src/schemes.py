"""
The collection of recursion schemes this library provides.

By convention:
`run` refers to the executor applying the provides (co-)algebra(s).
`data` refers to an object with a `map` method.
`alg` refers to an algebra that returns any value when processing `data`.
`seed` refers to any value.
`coalg` refers to a co-algebra that returns `data` when processing any value.
"""


# Folds
def cata(alg):
    """
    Breaks down a data structure recursively.
    """
    def run(data):
        return alg(data.map(run))
    return run


def para(alg):
    """
    Extending a catamorphism by exposing the original data structure.
    """
    def run(data):
        return alg(data.map(run), data)
    return run


def histo(alg):
    """
    Extending a catamorphism by exposing multiple steps of processing.
    """
    def run(data, history=()):
        new = alg(data, *history)
        return data.map(lambda dat: run(dat, (new,) + history))
    return run


# Unfolds
def ana(coalg):
    """
    Dual to a catamorphism, ana builds a data structure over which you can map.
    """
    def run(seed):
        return coalg(seed).map(run)
    return run


def apo(coalg):
    """
    Extending an anamorphism with the ability to halt.
    In this version, a boolean is paired with the value that indicates halting.
    """
    def run(seed):
        stop, v = coalg(seed)
        return v if stop else v.map(run)
    return run


def prop_apo(coalg, pred):
    """
    Like apo, but using a separate predicate function to determine stopping.
    """
    def run(seed):
        v = coalg(seed)
        return v if pred(v) else v.map(run)
    return run


def option_apo(coalg):
    """
    Like apo, but halting when the co-algebra function returns None.
    """
    def run(seed):
        optional_v = coalg(seed)
        return seed if optional_v is None else v.map(run)
    return run


class Insert:
    """
    A utility for taking multiple steps in a futumorphism.
    """
    def __init__(self, v): self.ins = v


def futu(coalg):
    """
    Unfolding like an anamorphism, but with multiple steps at a time with the use of Insert.
    The co-algebra is not run on elements wrapped in Insert.
    """
    def traverse_insert(fa):
        return fa.v.map(traverse_insert) if isinstance(fa, Insert) else run(fa)

    def run(seed):
        return coalg(seed).map(traverse_insert)
    return run


# Combinations
def hylo(alg, coalg):
    """
    Simultaneously unfolding with an anamorphism and folding with catamorphism.
    """
    def run(seed):
        return alg(coalg(seed).map(run))
    return run  # equivalent to lambda seed: cata(alg)(ana(coalg)(seed))


def chrono(alg, coalg):
    """
    Performing a histomorphism right after a futumorphism.
    """
    return lambda seed: histo(alg)(futu(coalg)(seed))
