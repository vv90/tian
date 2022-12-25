FROM ubuntu:20.04
ARG BIN_PATH

RUN apt-get update 
RUN apt-get -y install libpq-dev

# COPY /root/.local/bin/server-exe /app/server
COPY $BIN_PATH/server-exe /app/server

WORKDIR /app

EXPOSE 8081

CMD ["/app/server"]