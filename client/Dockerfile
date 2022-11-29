FROM node:16.15.1 as builder

# Install Elm
RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip elm.gz
RUN chmod +x elm
RUN mv elm /usr/local/bin/

# Install production-only dependencies
# Note: uglify-js after 3.9.4 uses different API, which requires refactoring
RUN npm i -g \
  uglify-js@3.9.4 \
  inline-source-cli@2.0.0

WORKDIR /build

# Install NPM dependencies
COPY package.json package-lock.json ./
RUN npm ci

# Copy sources
COPY src /build/src
COPY env /build/env
COPY elm.json /build/

# Compile Elm code
# RUN elm make --optimize --output="./bundle.uncompressed.js" src/Main.elm
RUN elm make src/Main.elm --optimize --output="./assets/inline/bundle.js"

# Copy assets and index page
COPY assets /build/assets
COPY index.src.html /build/

# Inline all inlineable assets
RUN bash -c 'inline-source --compress false --root ./build build/index.src.html > build/index.html'

# Remove just inlined assets
RUN rm -rf assets/inline


FROM nginx:1.23.0-alpine
WORKDIR /usr/share/nginx/html

# Copy bundle and assets to the Nginx container
COPY --from=builder /build/index.html .
COPY --from=builder /build/assets ./assets

EXPOSE 80