# This runs ci-like checks (typecheck, test, lint, format)
# and automatically applies fixes

# Fail on errors
set -e

# Server
cd server
# Fix formatting
ormolu --mode inplace $(find codegen seed app src test -name '*.hs')
# Fix linting errors
find codegen seed app src test -name '*.hs' |
  xargs -L1 hlint --refactor --refactor-options="--inplace"

stack clean
stack build --fast --pedantic --test
stack run codegen
cd ..

# Client
cd client
npm run precommit-fix
cd ..

echo "🔧 Precommit-fix complete! 🔧"
