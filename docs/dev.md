```sh
cd backend &&
  bash -c "ormolu --mode inplace \$(find codegen seed app src test -name '*.hs')" &&
  hlint . &&
  stack clean &&
  stack build --fast --pedantic --test &&
  stack run codegen &&
  cd ../frontend &&
  npm run precommit-fix &&
  cd ..
```
