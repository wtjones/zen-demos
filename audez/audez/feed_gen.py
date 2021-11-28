import datetime
import json
from pathlib import Path
from rfeed import *
from .format_utl import strfdelta


def generate_feed(metadata_path: str):
    path = Path(metadata_path)
    if path.is_dir():
        for child in path.iterdir():
            if child.suffix == ".json":
                _create_rss(child)
    else:
        _create_rss(path)


def _create_rss(json_path: Path):
    f = open(json_path, "r")

    rss_path = json_path.with_suffix(".rss")
    identifier = json_path.stem
    print(f"Creating feed for identifier: {identifier}")
    print(f"Loading json file: {f.name}")
    ia_item = json.loads(f.read())
    print(f"Loaded json file: {f.name}")
    f.close()
    items = []

    link = f"https://archive.org/download/{identifier}"

    img_link = next(
        f"{link}/{item['name']}"
        for item in ia_item["files"]
        if item["format"] == "JPEG"
    )

    for ia_file in ia_item["files"]:
        name = ia_file["name"] if "name" in ia_file else identifier
        title = ia_file["title"] if "title" in ia_file else name
        if ia_file["format"] == "VBR MP3":
            length = float(ia_file["length"])
            duration_formatted = strfdelta(
                datetime.timedelta(0, seconds=length), "%H:%M:%S"
            )

            item_link = f"{link}/{ia_file['name']}"

            itunes_item = iTunesItem(
                image=img_link,
                duration=duration_formatted,
                explicit="yes",
            )

            item1 = Item(
                title=title,
                link=item_link,
                description=title,
                guid=Guid(item_link),
                pubDate=datetime.datetime.fromtimestamp(int(ia_file["mtime"])),
                extensions=[itunes_item],
            )
            items.append(item1)

    itunes = iTunes(
        image=img_link,
        explicit="yes",
    )

    feed = Feed(
        title=ia_item["metadata"]["title"],
        link="http://www.example.com/rss",
        description=ia_item["metadata"]["title"],
        language="en-US",
        pubDate=datetime.datetime.fromtimestamp(int(ia_item["created"])),
        lastBuildDate=datetime.datetime.now(),
        items=items,
        extensions=[itunes],
    )

    print(f"Writing feed to {rss_path}")
    o = open(rss_path, "w")
    o.write(feed.rss())
    o.close()
