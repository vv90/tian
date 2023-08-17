set -e

cd server

DB_HOST=localhost \
    DB_PORT=5433 \
    DB_USER=admin \
    DB_PASS=admin \
    DB_NAME=cvdb \
    stack run server-exe
