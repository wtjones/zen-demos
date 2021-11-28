# Audez - An Internet Archive feed generator

## Init

```
poetry config virtualenvs.in-project true
poetry install
```

## Process

### Get Metadata

`poetry run get_metadata "identifier:the-kraken-wakes-by-john-wyndham" -o $TMPDIR/feed`


### Generate Feed

`poetry run generate_feed  $TMPDIR/feed/the-kraken-wakes-by-john-wyndham.json`

## TODO

- Support images per episode
- Support multiple image types
