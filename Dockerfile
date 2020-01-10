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

# install uglifyjs
RUN npm install uglify-js --global

# set the working directory for any RUN, CMD, ENTRYPOINT, COPY and ADD
# instructions that follows the WORKDIR instruction.
WORKDIR /app

# remember, our current working directory within the container is /app
# we now copy everything (except stuff listed in .dockerignore)
# from local machine to /app (in the container).
COPY . .

# install packages
RUN npm install

# build elm production code
RUN npm run deploy

# see where we are
RUN pwd

# see what files we have
RUN ls -lA dist/