# Tian

## Prerequisites

- VSCode with recommended extensions
- nvm
- node.js
- docker
- Elm compiler and toolkit
  - [Elm](https://guide.elm-lang.org/install/elm.html)
- On **Apple Silicon Macs**:
  - LLVM 12: `brew install llvm@12`
- Haskell compiler and toolkit:
  - [GHCup](https://www.haskell.org/ghcup/) (already includes `ghc`, `HLS`, `cabal` and `stack`).
  - Agree to enable `better integration of stack with GHCup` when prompted.
  - `cabal install hlint`
  - `cabal install apply-refact`
  - `cabal install ormolu`
- libpq (PostgreSQL client library)
  - On **Mac**:
    - `brew install libpq`
    - `brew link --force libpq`

```sh
cd client
nvm use; nvm install
```

## Development

### Client

```sh
cd client
npm install
npm start
```

### Server

```sh
cd server
stack build
stack run server-exe
```

### Before commit

```sh
bash precommit-fix.sh
```

## Useful links

- [Elm language guide](https://guide.elm-lang.org/)
- [Elm patterns](https://sporto.github.io/elm-patterns/index.html)
- [Elm-ui patterns](https://korban.net/elm/elm-ui-patterns/button)
- [Elm-ui community cookbook](https://github.com/rofrol/elm-ui-cookbook)
- [Haskell] (https://www.haskell.org/documentation/)
- [Haskell wiki](https://wiki.haskell.org/Haskell)

## Preparing the data

Import digital elevation map

```sh
raster2pgsql -s 4326 -I -C -M -F -t 100x100 ASTGTMV003_N45E005_dem.tif | psql -h localhost -d cvdb -p 5433 -U admin
```
