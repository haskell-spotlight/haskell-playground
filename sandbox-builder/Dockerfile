FROM visortelle/haskell-playground-sandbox:main

COPY . .

USER root
RUN chown -R haskeller:haskeller .

USER haskeller

RUN ./build.sh
