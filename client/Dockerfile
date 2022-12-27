FROM node:16.15.1 as builder

# Install Elm!
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
RUN elm make src/Main.elm --optimize --output="./assets/bundle.js"

RUN uglifyjs "./assets/bundle.js" --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters=true,keep_fargs=false,unsafe_comps=true,unsafe=true,passes=2' --output="./assets/bundle.js" && uglifyjs "./assets/bundle.js" --mangle --output="./assets/bundle.js"

# Copy assets and index page
COPY assets /build/assets
COPY index.html /build/

# Inline all inlineable assets
# RUN bash -c 'inline-source --compress false --root ./ index.src.html > index.html'

# Remove just inlined assets
# RUN rm -rf assets/inline


FROM nginx:1.23.0-alpine
WORKDIR /usr/share/nginx/html

# Copy bundle and assets to the Nginx container
COPY --from=builder /build/index.html .
COPY --from=builder /build/assets ./assets

# Copy nginx config
COPY ./configs/nginx.conf /etc/nginx/conf.d/default.conf

EXPOSE 80
