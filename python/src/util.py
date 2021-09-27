from dataclasses import dataclass, is_dataclass, fields, replace


class AutoFunctor(type):
    def __init__(cls, name, bases, dct):
        if bases:
            cls.map = lambda self, func: replace(self, **{
                field.name: func(getattr(self, field.name))
                for field in fields(self)
                if field.type is bases[0]
            })

        super().__init__(name, bases, dct)
