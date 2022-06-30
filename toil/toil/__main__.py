from .id_generator import *
from dataclasses import dataclass
from abc import ABC
from pprint import pprint
from .app import *
from .component import *


@dataclass
class Position(Component):
    x: int
    y: int


@dataclass
class Person(Component):
    name: str


def some_func(my_int: int) -> str:
    return str(my_int)


def query_func(query: Query[Person, Position]):
    return Person('foo1')


def main():
    # id = IdGenerator.generate_id()
    # print(f"test {id}")
    # id = IdGenerator.generate_id()
    # print(f"test {id}")
    # pd = {}
    # pod = {}
    # pd[1] = Position(4, 5)
    # pod[1] = Person("taco")
    # pd[2] = Person("foo")

    # world = {}
    # components = {}
    # components2 = {}

    # components[1] = Person("abc")
    # components[2] = Person("abcd")
    # world[Person.__name__] = components
    # components2[1] = Position(5, 6)
    # world[Position.__name__] = components2
    # pprint(world)

    app = App()
    # app.add_startup_system(some_func)
    app.add_startup_system(query_func)
    app.run()


if __name__ == '__main__':
    main()
