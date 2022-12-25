FROM ubuntu:20.04


RUN apt-get update 
RUN apt-get -y install libpq-dev

# COPY /root/.local/bin/server-exe /app/server
COPY --from=BIN_PATH_CONTEXT server-exe /app/server

WORKDIR /app

EXPOSE 8081

CMD ["/app/server"]