from pathlib import os
import urllib.request
from internetarchive import search_items


def get_metadata(query: str, output: str):
    for i in search_items(query):
        id = i["identifier"]
        if output != None:
            os.makedirs(output, exist_ok=True)
            contents = (
                urllib.request.urlopen(
                    f'https://www.archive.org/metadata/{i["identifier"]}'
                )
                .read()
                .decode("utf-8")
            )
            out_file = os.path.join(output, f"{id}.json")

            o = open(out_file, "w+")
            print(f"Writing {out_file}...")
            o.write(contents)
            o.close()
