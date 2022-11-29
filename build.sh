# exit on the first failure
set -e

# Because of codegen, we cant't simply ask docker-compose to build everything.
# Instead, we need a step-by step approach (almost like in GitHub Actions, but with local specifics).

# Build backend-artifacts
docker build -f server/Dockerfile.artifacts -t artifacts server

# Create container from image without running it to extract artifacts
id=$(docker create artifacts)

# Save stack depenencies cache
# TODO: check md5 sum before extracting
# to avoid copying the same archive to the host again (useful only for frequent local docker builds)
docker cp $id:/root/stack_dependencies_cache.tar.gz - >./server/cache/stack_dependencies_cache.tar.gz.tar

# Replace the existing generated code with just generated
# Note: we could also do it via docker,
# but it will require anyone to build backend-artifacts before building the frontend
# which will take 20-40 minutes.

# rm -rf client/src/Api
# docker cp $id:/client/src/Api - > client/src/Api.tar
# tar -xf client/src/Api.tar -C client/src
# rm client/src/Api.tar

# Remove the container after extracting artifacts
docker rm -v $id

# Build backend
docker-compose -f docker-compose.yml build server

# - pack server binary to a new image

# - linting all the frontend code with elm-review and elm-format
# - remove generated frontend code
# - extract generated elm files from backend-artifacts and put them to the frontend codebase
# - format and autofix the generated code
# - run frontend tests
# - compile and pack frontend:

# Build frontend
docker-compose -f docker-compose.yml build client

# Stop everything
docker-compose -f docker-compose.yml down -v
