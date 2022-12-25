FROM ubuntu:20.04


RUN apt-get update 
RUN apt-get -y install libpq-dev

# COPY /root/.local/bin/server-exe /app/server
COPY --from=bin_path_context server-exe /app/server

WORKDIR /app

EXPOSE 8081

CMD ["/app/server"]