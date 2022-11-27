FROM artifacts as builder

FROM ubuntu:20.04

COPY --from=builder /root/.local/bin/server /app/server

WORKDIR /app

EXPOSE 8000

CMD ["/app/server"]