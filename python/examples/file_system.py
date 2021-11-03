from dataclasses import dataclass, field
from typing import Any

from core import AutoFunctor
from schemes import *


# Structures
class FileSystem(metaclass=AutoFunctor):
    pass


@dataclass
class File(FileSystem):
    name: str
    data: Any = field(default=None)


@dataclass
class Dir(FileSystem):
    name: str
    content: list[FileSystem] = field(default_factory=list)

    def map(self, f):
        return Dir(self.name, list(map(f, self.content)))


# Algebras
def compress_alg(fa):
    if isinstance(fa, File): return fa
    elif isinstance(fa, Dir):
        if len(fa.content) == 1:
            fb = fa.content[0]
            if isinstance(fb, Dir):
                return Dir(f"{fa.name}/{fb.name}", fb.content)
        return fa


def insert(path, content):
    def insert_coalg(fa, trace):
        if isinstance(fa, File): return fa
        elif isinstance(fa, Dir):
            if path == '/'.join(h.name for h in reversed((fa,) + trace)):
                return Dir(fa.name, fa.content + content)
            return fa
    return insert_coalg


def find(directory, query):
    def find_coalg(fa, todo):
        if isinstance(fa, File):
            path = f"{'/'.join(t.name for t in reversed(todo))}/{fa.name}"
            if path.startswith(directory) and query in fa.name: return [path]
            else: return []
        elif isinstance(fa, Dir): return sum(fa.content, [])
    return find_coalg


# Usage
def run():
    t = Dir("root", [
        Dir(".github", [Dir("workflows", [Dir("something")])]),
        Dir("examples", [File("Ex1"), File("Ex2")]),
        File(".gitignore")
    ])

    st = [
        File("Test1"),
        File("Test2"),
        Dir("SD", [File("SDTest")])
    ]

    print(cata(compress_alg)(t))

    print(prothesi(find("root/examples", "Ex"))(t))

    print(ichno(insert("root/.github/workflows", st))(t))


if __name__ == '__main__':
    run()
