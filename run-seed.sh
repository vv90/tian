set -e

cd server

DB_HOST=localhost \
    DB_PORT=5432 \
    DB_USER= \
    DB_PASS= \
    DB_NAME=elevations \
    stack run seed-exe
