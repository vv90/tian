# server

- Rebuild backend watching files (PostgreSQL must be running and `.env` file must be filled with connection details)

```sh
stack build --file-watch --fast
```

- Run server (does not reload on code change, needs manual stopping and re-running). The `--fast` flag is omitted because the better runtime performance is preferred over the short build time when running the server.

```sh
stack run server
```

- Run codegen locally

```sh
stack run codegen
```

- Run tests, watching files

```sh
stack test --file-watch --fast
```
