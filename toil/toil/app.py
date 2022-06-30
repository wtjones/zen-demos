from typing import get_args
from pprint import pprint

from typing import Generic, ParamSpec, TypeVar
from dataclasses import dataclass
from optparse import OptionParser
from toil.world import *

P = ParamSpec('P')


@dataclass
class Query(Generic[P]):
    result: str


class App:

    # def __init__(self):
    #     pass
    startup_systems = []

    world: World

    def add_startup_system(self, func):
        self.startup_systems.append(func)

    def add_system(self, func):
        pass

    def run(self):

        func = self.startup_systems[0]

        print(func.__annotations__['query'])
        print(type(func.__annotations__['query']))
        query_type = func.__annotations__['query']
        #inspect.getmembers(query_type, predicate=inspect.isfunction)
        print(type(get_args(query_type)[0][0]))
        print(get_args(query_type)[0][0])
