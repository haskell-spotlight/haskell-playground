# Sandbox API builder
FROM ubuntu:20.04 as api-executable

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

COPY --chown=haskeller:haskeller ./api/cabal.project.freeze .
COPY --chown=haskeller:haskeller ./api/api.cabal .
RUN cabal update && cabal build --only-dependencies

COPY --chown=haskeller:haskeller ./api .
RUN cabal install --installdir="/home/haskeller/bin/"

###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################

# Sandbox base image
FROM ubuntu:20.04

USER root

# Create haskeller user
RUN useradd -rm -d /home/haskeller -s /bin/bash -g root -G sudo -u 1001 haskeller \
  && addgroup haskeller && adduser haskeller haskeller
WORKDIR /home/haskeller/sandbox

ENV DEBIAN_FRONTEND=noninteractive
ENV COLORTERM=truecolor
RUN apt-get update && apt-get install -y \
  software-properties-common build-essential libgmp-dev libtinfo-dev zlib1g-dev liblzma-dev lzma-dev \
  dumb-init \
  ruby python3 python3-pip \
  git curl wget zsh gettext-base nginx zip unzip

# Install NodeJS
RUN curl -sL https://deb.nodesource.com/setup_16.x -o nodesource_setup.sh \
  && bash nodesource_setup.sh \
  && apt-get install -y nodejs

# Install optional programs
RUN apt-get install -y jq httpie tree ripgrep silversearcher-ag

RUN chmod -R 777 /etc/nginx /var/log/nginx /var/lib/nginx /var/run

USER haskeller
WORKDIR "/home/haskeller"

# Install oh-my-zsh for user's comfort
RUN sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
ENV SHELL=/bin/zsh

# Configure editors
## TODO - neovim and probably emacs

USER root
RUN apt-get install -y neovim nano micro
USER haskeller

# nano
RUN curl https://raw.githubusercontent.com/scopatz/nanorc/ae1e6021886a0b4215e1ca8617c6d746b5a957ae/install.sh | sh \
  && echo "include $install_path/*.nanorc" >> ~/.nanorc

ENV EDITOR=micro

# Install Haskell build tools
RUN mkdir ~/bin
ENV PATH="$PATH:/home/haskeller/bin:/home/haskeller/.ghcup/bin"

RUN curl -L --output ~/bin/ghcup https://downloads.haskell.org/~ghcup/0.1.17.4/x86_64-linux-ghcup-0.1.17.4 \
  && chmod 700 ~/bin/ghcup

RUN ghcup install ghc 8.10.7 && ghcup set ghc 8.10.7
RUN ghcup install stack 2.7.3 && ghcup set stack 2.7.3
RUN ghcup install cabal 3.6.2.0 && ghcup set cabal 3.6.2.0 && cabal update

# Install Gotty to expose terminal to web
RUN curl -L --output /tmp/gotty.tar.gz https://github.com/haskell-spotlight/gotty/releases/download/latest/gotty_latest_linux_amd64.tar.gz \
  && tar -vxf /tmp/gotty.tar.gz \
  && mv ./gotty ~/bin/
COPY ./.gotty /home/haskeller/.gotty

ENV HSPG_SANDBOX_ROOT=/home/haskeller/sandbox
ENV HSPG_NGINX_CONFIG_PATH=/etc/nginx/nginx.conf

# Copy sandbox base api executable
COPY --from=api-executable --chown=haskeller:haskeller /home/haskeller/bin/api /home/haskeller/bin/api

# Copy UI
ENV HSPG_UI_DIST=/home/haskeller/ui
COPY --chown=haskeller:haskeller ./ui /home/haskeller/ui

ENV HSPG_API_PORT=8080

ENV HSPG_NGINX_PORT=8090
EXPOSE 8090

ENV DUMB_INIT_SETSID=0
ENTRYPOINT ["/usr/bin/dumb-init", "--"]
CMD ["api"]
