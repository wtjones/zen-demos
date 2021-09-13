import click
from pathlib import os
from internetarchive import search_items
from pprint import pprint
import urllib.request


@click.command()
@click.argument("query", )
@click.option("--output", "-o", default=None, type=str)
def get_metadata(query: str, output: str):
    for i in search_items(query):
        id = i["identifier"]
        print(f'{i["identifier"]}')
        if output != None:
            os.makedirs(output, exist_ok=True)
            contents = urllib.request.urlopen(
                f'https://www.archive.org/metadata/{i["identifier"]}').read().decode('utf-8')
            out_file = os.path.join(output, f'{id}.json')

            o = open(out_file, "w+")
            print(contents)
            print(f'Writing {out_file}...')
            o.write(contents)
            o.close()
