# Ubuntu Focal Fossa 20.04 linux/amd64
FROM ubuntu@sha256:57df66b9fc9ce2947e434b4aa02dbe16f6685e20db0c170917d4a1962a5fe6a9

USER root

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y git curl build-essential libgmp-dev vim zsh gettext-base nginx

RUN chmod -R 777 /etc/nginx /var/log/nginx /var/lib/nginx /var/run

# Create haskeller user
RUN useradd -rm -d /home/haskeller -s /bin/bash -g root -G sudo -u 1001 haskeller \
  && addgroup haskeller && adduser haskeller haskeller
USER haskeller
WORKDIR "/home/haskeller"

# Install oh-my-zsh for user's comfort
RUN sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended

RUN mkdir ~/bin
ENV PATH="$PATH:/home/haskeller/bin:/home/haskeller/.ghcup/bin"

# Install GHCup
RUN curl -L --output ~/bin/ghcup https://downloads.haskell.org/~ghcup/0.1.17.4/x86_64-linux-ghcup-0.1.17.4 \
  && chmod 700 ~/bin/ghcup

# Install GHC
RUN ghcup install ghc 8.10.7 && ghcup set ghc 8.10.7

# Install Stack
RUN ghcup install stack 2.7.3 && ghcup set stack 2.7.3

# Install Cabal
RUN ghcup install cabal 3.6.2.0 && ghcup set cabal 3.6.2.0

# Install Gotty to expose terminal to web
RUN curl -L --output /tmp/gotty.tar.gz https://github.com/sorenisanerd/gotty/releases/download/v1.3.0/gotty_v1.3.0_linux_amd64.tar.gz \
  && tar -vxf /tmp/gotty.tar.gz \
  && mv ./gotty ~/bin/

# Nginx
EXPOSE 8080

WORKDIR /home/haskeller/sandbox

COPY ./template.nginx.conf /etc/nginx/template.nginx.conf
COPY ./entrypoint.sh /home/haskeller/
ENTRYPOINT [ "/home/haskeller/entrypoint.sh" ]
