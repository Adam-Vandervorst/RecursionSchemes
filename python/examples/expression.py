from dataclasses import dataclass

from core import AutoFunctor
from schemes import *


# Structures
class Expr(metaclass=AutoFunctor):
    pass


@dataclass
class Var(Expr):
    pass


@dataclass
class Zero(Expr):
    pass


@dataclass
class One(Expr):
    pass


@dataclass
class Neg(Expr):
    x: Expr


@dataclass
class Exp(Expr):
    x: Expr


@dataclass
class Add(Expr):
    x: Expr
    y: Expr


@dataclass
class Prod(Expr):
    x: Expr
    y: Expr


# Algebras
def string_alg(fa):
    if isinstance(fa, Var): return 'x'
    elif isinstance(fa, Zero): return '0'
    elif isinstance(fa, One): return '1'
    elif isinstance(fa, Neg): return f'-{fa.x}'
    elif isinstance(fa, Exp): return f'e^({fa.x})'
    elif isinstance(fa, Add): return f'({fa.x} + {fa.y})'
    elif isinstance(fa, Prod): return f'({fa.x}*{fa.y})'


def eval_in(value):
    def eval_alg(fa):
        if isinstance(fa, Var): return value
        elif isinstance(fa, Zero): return 0
        elif isinstance(fa, One): return 1
        elif isinstance(fa, Neg): return -fa.x
        elif isinstance(fa, Exp): return 2.71828**fa.x
        elif isinstance(fa, Add): return fa.x + fa.y
        elif isinstance(fa, Prod): return fa.x*fa.y
    return eval_alg


def diff_alg(fa, ofa):
    if isinstance(fa, Var): return One()
    elif isinstance(fa, Zero): return Zero()
    elif isinstance(fa, One): return Zero()
    elif isinstance(fa, Neg): return Neg(fa.x)
    elif isinstance(fa, Exp): return Prod(Exp(ofa.x), fa.x)
    elif isinstance(fa, Add): return Add(fa.x, fa.y)
    elif isinstance(fa, Prod): return Add(Prod(ofa.x, fa.y), Prod(fa.x, ofa.y))


def simplify_alg(fa):
    if isinstance(fa, Neg):
        if isinstance(fa.x, Neg): return fa.x.x
    elif isinstance(fa, Exp):
        if isinstance(fa.x, Zero): return One()
    elif isinstance(fa, Add):
        if isinstance(fa.x, Zero): return fa.y
        elif isinstance(fa.y, Zero): return fa.x
        elif isinstance(fa.y, Zero): return fa.x
    elif isinstance(fa, Prod):
        if isinstance(fa.x, Zero): return fa.x
        elif isinstance(fa.y, Zero): return fa.y
        elif isinstance(fa.x, One): return fa.y
        elif isinstance(fa.y, One): return fa.x
        elif isinstance(fa.x, Neg) and isinstance(fa.x.x, One): return Neg(fa.y)
        elif isinstance(fa.y, Neg) and isinstance(fa.y.x, One): return Neg(fa.x)
    return fa


# Usage
def run():
    # -x2 + -e^(1 - x)
    expr = Add(Neg(Prod(Var(), Var())), Neg(Exp(Add(One(), Neg(Var())))))

    print(cata(eval_in(1.2))(expr))

    print(cata(string_alg)(para(diff_alg)(expr)))
    # (-((x*1) + (1*x)) + -(e^((1 + -x))*(0 + -1)))
    print(cata(string_alg)(cata(simplify_alg)(para(diff_alg)(expr))))


if __name__ == '__main__':
    run()
