import click
from . import feed_gen
from . import ia_reader


@click.command()
@click.argument("query")
@click.option("--output", "-o", default=None, type=str)
def get_metadata(query: str, output: str):
    ia_reader.get_metadata(query, output)


@click.command()
@click.argument("metadata_path", type=str)
def generate_feed(metadata_path: str):
    feed_gen.generate_feed(metadata_path)
