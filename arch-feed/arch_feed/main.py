import datetime
import json
from pathlib import os
import urllib.request
from pathlib import Path
from internetarchive import search_items
import click
from rfeed import *


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

@click.command()
@click.argument("metadata_path", type=str)
def generate_feed(metadata_path: str):
    path = Path(metadata_path)
    if path.is_dir():
        for child in path.iterdir():
            if child.suffix == ".json":
                create_feed(child)
    else:
        create_feed(path)


def create_feed(path: Path):
    f = open(path, "r")

    rss_path = path.with_suffix('.rss')
    identifier = path.stem
    print(f"Creating feed for identifier: {identifier}")
    print(f"Loading json file: {f.name}")
    ia_item = json.loads(f.read())
    print(f"Loaded json file: {f.name}")
    f.close()
    items = []

    link = f"https://archive.org/download/{identifier}"
    for ia_file in ia_item['files']:
        name = ia_file['name'] if 'name' in ia_file else identifier
        title = ia_file['title'] if 'title' in ia_file else name
        if ia_file['format'] == "VBR MP3":


            item_link = f"{link}/{ia_file['name']}"
            item1 = Item(
                title = title,
                link = item_link,
                description = title,
                guid = Guid(item_link),
                pubDate = datetime.datetime.fromtimestamp(int(ia_file['mtime'])))
            items.append(item1)

    feed = Feed(
        title = ia_item["metadata"]["title"],
        link = "http://www.example.com/rss",
        description = ia_item["metadata"]["title"],
        language = "en-US",
        pubDate=datetime.datetime.fromtimestamp(int(ia_item['created'])),
        lastBuildDate = datetime.datetime.now(),
        items = items)

    print(f"Writing feed to {rss_path}")
    o = open(rss_path, "w")
    o.write(feed.rss())
    o.close()

