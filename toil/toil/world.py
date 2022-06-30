from typing import get_args
from pprint import pprint

from typing import Generic, ParamSpec, TypeVar
from dataclasses import dataclass
from optparse import OptionParser
import inspect

class World:

    # def __init__(self):
    #     pass


    components = {}


    def run(self):

        func = self.startup_systems[0]

        print(func.__annotations__['query'])
        print(type(func.__annotations__['query']))
        query_type = func.__annotations__['query']
        #inspect.getmembers(query_type, predicate=inspect.isfunction)
        print(type(get_args(query_type)[0][0]))
        print(get_args(query_type)[0][0])
