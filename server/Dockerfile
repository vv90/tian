FROM artifacts as builder

FROM ubuntu:20.04

RUN apt-get update 
RUN apt-get -y install libpq-dev

COPY --from=builder /root/.local/bin/server-exe /app/server

WORKDIR /app

EXPOSE 8000

CMD ["/app/server"]