# Metadata Server

## Running

For detailed instructions on the metadata ecosystem, please see [the docs](./docs/main.org).

## Development

```
# Launch a ghcid session for the given target
make dev target=lib:metadata-lib
make dev target=exe:metadata-server
make dev target=exe:metadata-webhook
# Launch a ghci session for the given target
make repl target=lib:metadata-lib
```
