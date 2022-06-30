

from dataclasses import dataclass
from typing import *
from abc import *
from toil.component import *
from toil.id_generator import IdGenerator
from toil.world import *

T = TypeVar('T', bound=Component)


class Command(ABC):
    @abstractmethod
    def apply(self, world: World):
        ...


# @dataclass
# class InsertCommand(Command, Generic[T]):
#     id: int
#     component: T

@dataclass
class SpawnCommand(Command):
    id: int

    def apply(self, world: World):
        pass


@dataclass
class InsertCommand(Command):
    id: int
    component: Component

    def apply(self, world: World):
        pass
# should just be single component?


# @dataclass
# class SpawnCommand:
#     id: int
#     #components: List[Component]
#     components: Dict[Type, Component]


class EntityCommandBuilder():
    id: int = 0
    commands: List[Command]

    def __init__(self):
        self.id = IdGenerator.generate_id()
        #self.command = SpawnCommand(id, {})
        self.commands = []

    def spawn(self):
        self.commands.append(SpawnCommand(self.id))
        return self

    def insert(self, component: Component):
        print(f"type name is {type(component).__name__}")
        #self.command.components[type(component)] = component
        self.commands.append(InsertCommand(self.id, component))
        return self


class Commands():

    def __init__(self):
        self.commands = []

    def spawn(self):
        builder = EntityCommandBuilder()
        # self.commands.append(builder.command)
        builder.spawn()
        self.commands = builder.commands
        return builder
