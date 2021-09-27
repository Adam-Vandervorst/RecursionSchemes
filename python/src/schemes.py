# Folds
def cata(alg):
    def run(data):
        return alg(data.map(run))
    return run


def para(alg):
    def run(data):
        return alg(data.map(run), data)
    return run


def histo(alg):
    def run(data, history=()):
        new = alg(data, *history)
        return data.map(lambda dat: run(dat, (new,) + history))
    return run


# Unfolds
def ana(coalg):
    def run(seed):
        return coalg(seed).map(run)
    return run


def apo(coalg):
    def run(seed):
        stop, v = coalg(seed)
        return v if stop else v.map(run)
    return run


def prop_apo(coalg, pred):
    def run(seed):
        v = coalg(seed)
        return v if pred(v) else v.map(run)
    return run


def option_apo(coalg):
    def run(seed):
        optional_v = coalg(seed)
        return seed if optional_v is None else v.map(run)
    return run


class Insert:
    def __init__(self, v): self.ins = v


def futu(coalg):
    def traverse_insert(fa):
        return fa.v.map(traverse_insert) if isinstance(fa, Insert) else run(fa)

    def run(seed):
        return coalg(seed).map(traverse_insert)
    return run


# Combinations
def hylo(alg, coalg):
    def run(seed):
        return alg(coalg(seed).map(run))
    return run  # equivalent to lambda seed: cata(alg)(ana(coalg)(seed))


def chrono(alg, coalg):
    return lambda seed: histo(alg)(futu(coalg)(seed))
