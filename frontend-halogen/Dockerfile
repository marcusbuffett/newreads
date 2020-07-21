FROM ubuntu:18.04

# Install dependencies.
RUN apt-get update && \
  apt-get install --assume-yes curl gcc libgmp-dev libpq-dev make xz-utils zlib1g-dev gnupg2

RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list

RUN apt-get update && \
  apt-get install --assume-yes yarn



RUN curl -O https://nodejs.org/dist/v12.18.0/node-v12.18.0-linux-x64.tar.xz
RUN tar xvf node-v12.18.0-linux-x64.tar.xz
RUN mv node-v12.18.0-linux-x64 node
ENV PATH="/node/bin:${PATH}"
# RUN curl --silent -o- https://raw.githubusercontent.com/creationix/nvm/master/install.sh | bash
# RUN nvm install && nvm use

# RUN apt-get install --assume-yes ghc

# RUN addgroup -S docker && adduser -S docker -G docker

# USER docker

# RUN curl -sSL https://get.haskellstack.org/ | sudo sh

# RUN apk add git

# RUN git clone https://github.com/purescript/spago spago

# RUN ln -s /usr/lib/libncursesw.so.6 /usr/lib/libtinfo.so.6

# WORKDIR spago

# RUN apk add libc6-compat

# RUN stack install

RUN apt-get install --assume-yes git

RUN useradd -ms /bin/bash docker

USER docker

RUN mkdir /home/docker/.npm-packages

RUN npm config set prefix /home/docker/.npm-packages

RUN npm install -g parcel spago purescript

ENV PATH="/home/docker/.npm-packages/bin:${PATH}"

RUN mkdir /home/docker/project

WORKDIR /home/docker/project

COPY packages.dhall .
COPY spago.dhall .
COPY package.json .
RUN spago build
RUN yarn install
# Build project.
COPY --chown=docker . .

RUN spago build

RUN yarn build

RUN cp -r public/* dist

USER root

ENV PATH="/home/docker/.npm-packages/bin:${PATH}"

RUN mkdir -p /etc/caddy/

WORKDIR /home/docker/project

# Run project.
RUN cp -r dist /site

USER root


RUN apt-get install --assume-yes wget
RUN wget https://github.com/caddyserver/caddy/releases/download/v2.0.0/caddy_2.0.0_linux_amd64.deb
RUN dpkg -i caddy_2.0.0_linux_amd64.deb

CMD caddy run --config /root/.caddy_config/Caddyfile


