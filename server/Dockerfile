FROM ubuntu:22.04

# If we don't set locale this way and try to print emoji to stdout, the server will crash.
ENV LANG=C.UTF-8

# ARG SOME_VERSION

# Note: 
# ca-certificates solves an issue with some SSL certificates 
# libtinfo5 makes the binary ever run
# curl for healthcheck (temporary dirty solution)

RUN apt-get update && apt-get install -y ca-certificates libtinfo5 curl libpq-dev
RUN update-ca-certificates

COPY server-exe /app/server
# RUN bash -c "echo $SOME_VERSION > /app/version"

WORKDIR /app

EXPOSE 8081

CMD ["/app/server"]
