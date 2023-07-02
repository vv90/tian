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

## Deployment

### prerequisities:

- stack (https://docs.haskellstack.org/en/stable/)
- postgress (specifically, libpq library)
- elm compiler (https://github.com/elm/compiler)
- elm-format (https://github.com/avh4/elm-format/)
- npm

### steps to build and deploy

- initialize DB using `db/init.sql` script
- run BE tests
  ```sh
  stack test
  ```
- generate FE api types
  ```sh
  stack run codegen
  ```
- build BE executables
  ```sh
  stack install
  ```
- build FE

  ```sh
  elm make
  ```

### Formatting

```sh
bash -c "ormolu --mode inplace \$(find app codegen seed src test -name '*.hs')"
```
