"""
Provides basic utilities for dealing with recursion schemes in Python.
"""
from dataclasses import dataclass, fields, replace


@dataclass
class CoFree:
    a: 'A'
    fa: 'F[CoFree[F, A]]'


class Free:
    pass


@dataclass
class Pure(Free):
    a: 'A'


@dataclass
class Bind(Free):
    fa: 'F[Free[F, A]]'


@dataclass
class Insert:
    """
    A utility for taking multiple steps in a futumorphism.
    Elements in `Insert` are not mapped, only traversed.
    """
    fa: 'F[A]'


class AutoFunctor(type):
    """
    This slapdash metaclass provides a mapping function for child classes.
    Specifically, the `map` method takes a function and applies it to all fields with type annotations recursive to the parent class.
    Only works on dataclasses and does not overwrite existing `map` methods.
    """
    def __init__(cls, name, bases, d):
        if bases and not hasattr(cls, "map"):
            cls.map = lambda self, func: replace(self, **{
                field.name: func(getattr(self, field.name))
                for field in fields(self)
                if field.type is bases[0]
            })

        super().__init__(name, bases, d)
