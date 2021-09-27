"""
Provides basic utilities for dealing with recursion schemes in Python.
"""
from dataclasses import fields, replace


class AutoFunctor(type):
    """
    This slapdash metaclass provides a mapping function for child classes.
    Specifically, the `map` method takes a function and applies it to all fields with type annotations recursive to the parent class.
    Only works on dataclasses.
    """
    def __init__(cls, name, bases, dct):
        if bases:
            cls.map = lambda self, func: replace(self, **{
                field.name: func(getattr(self, field.name))
                for field in fields(self)
                if field.type is bases[0]
            })

        super().__init__(name, bases, dct)
