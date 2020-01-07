# set base image as alpine
FROM alpine:3.11.2 AS builder

# set maintainer
MAINTAINER Jwan Khalaf <jwan.khalaf@outlook.com>

# download the elm compiler and extract it to /user/local/bin/elm
RUN wget -O - 'https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz' \
    | gunzip -c >/usr/local/bin/elm

# make the elm compiler executable
RUN chmod +x /usr/local/bin/elm

# update remote repositories
RUN apk update

# install nodejs
RUN apk add --update nodejs npm

# install curl
RUN apk add curl

# install uglifyjs
RUN npm install uglify-js --global

# set the working directory for any RUN, CMD, ENTRYPOINT, COPY and ADD
# instructions that follows the WORKDIR instruction.
WORKDIR /app

# remember, our current working directory within the container is /app
# we now copy everything (except stuff listed in .dockerignore)
# from local machine to /app (in the container).
COPY . .

# check to make sure container can reach package.elm-lang.org
RUN ping -c 5 package.elm-lang.org

# do a curl request to url that was giving error
RUN curl https://package.elm-lang.org/packages/elm/file/1.0.5/endpoint.json

# build elm production code
RUN elm make src/app/Main.elm --optimize --output=elm.js
