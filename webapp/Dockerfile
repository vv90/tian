FROM nginx:1.25.0-alpine
WORKDIR /usr/share/nginx/html

# Install jq in order to manipulate json files from script
RUN apk update && apk add jq

# Copy nginx config
COPY ./configs/nginx.conf /etc/nginx/conf.d/default.conf

# Add script to update config from env variables on container startup
COPY ./configs/0-update-config-from-env.sh /docker-entrypoint.d/

# Make the script executable, otherwise it won't run
RUN chmod +x /docker-entrypoint.d/0-update-config-from-env.sh

# Copy bundle and assets
COPY ./dist /usr/share/nginx/html

EXPOSE 80
