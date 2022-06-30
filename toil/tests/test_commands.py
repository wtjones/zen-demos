from toil.commands import *
from toil.component import *
from pprint import pprint


@dataclass
class Person(Component):
    name: str


@dataclass
class Position(Component):
    x: int
    y: int


def test_spawn_new():
    sut = Commands()
    assert sut.spawn().id > 0


def test_spawn_with_components():
    sut = Commands()

    sut.spawn().insert(Person("Guy")).insert(Position(4, 5))
    #assert len(sut.commands[0].components) == 2
    pprint(sut.commands)
    # print(sut.commands[0].components.keys)
