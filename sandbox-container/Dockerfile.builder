# Sandbox API builder
FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y curl \
  software-properties-common build-essential libgmp-dev libtinfo-dev zlib1g-dev liblzma-dev lzma-dev

# Create haskeller user
RUN useradd -rm -d /home/haskeller -s /bin/bash -g root -G sudo -u 1001 haskeller \
  && addgroup haskeller && adduser haskeller haskeller

USER haskeller
WORKDIR /home/haskeller/src

RUN mkdir ~/bin
ENV PATH="$PATH:/home/haskeller/bin:/home/haskeller/.ghcup/bin"

RUN curl -L --output ~/bin/ghcup https://downloads.haskell.org/~ghcup/0.1.17.4/x86_64-linux-ghcup-0.1.17.4 \
  && chmod 700 ~/bin/ghcup

RUN ghcup install ghc 8.10.7 && ghcup set ghc 8.10.7
RUN ghcup install cabal 3.6.2.0 && ghcup set cabal 3.6.2.0

# It should be in another place, but it is here to cache haskell deps. Without it build takes ~15 minutes more.
RUN cabal update
COPY --chown=haskeller:haskeller ./api/cabal.project.freeze .
COPY --chown=haskeller:haskeller ./api/api.cabal .
RUN cabal build --only-dependencies
