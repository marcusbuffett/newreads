FROM haskell:latest

# Install dependencies.
RUN apt-get update && \
  apt-get install --assume-yes curl gcc libgmp-dev libpq-dev make xz-utils zlib1g-dev git locales
RUN locale-gen en_US.UTF-8

RUN adduser --disabled-password --gecos '' docker
RUN adduser docker sudo
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

USER docker

# Install GHC.
RUN mkdir /home/docker/project
# COPY stack.yaml /project
RUN stack setup && \
  stack exec -- ghc --version

# Install dependencies.
COPY package.yaml /home/docker/project
COPY stack.yaml /home/docker/project
# COPY nwdir-server.cabal /home/docker/project
WORKDIR /home/docker/project
RUN stack setup
RUN stack build --only-dependencies

# Build project.
COPY --chown=docker . /home/docker/project
RUN stack build --allow-different-user --copy-bins --local-bin-path .

# Run project.
USER root
ENV HOST 0.0.0.0
ENV PORT 4202
ENV DB_HOST "db"
ENV DB_PORT "5432"
ENV CACHE_PATH = /var/cache/newreads
EXPOSE 4202
ARG SERVER_ARGS
ENV SERVER_ARGS $SERVER_ARGS
RUN echo "cache killer 2"
RUN echo -- $SERVER_ARGS
CMD ./start.sh
