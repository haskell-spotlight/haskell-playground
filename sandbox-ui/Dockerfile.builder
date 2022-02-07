FROM node:16

WORKDIR /src

COPY ./package.json .
COPY ./package-lock.json .
RUN npm ci

COPY . .

ENTRYPOINT [ "npm", "run", "build" ]
