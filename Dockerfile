FROM node:14

ENV DEBIAN_FRONTEND noninteractive
ENV USER=node
ENV NODE_ENV docker

# You can not use `${USER}` here, but reference `/home/node`.
ENV PATH="/home/node/.npm-global/bin:${PATH}"
# 👉 The `--global` install dir
ENV NPM_CONFIG_PREFIX="/home/node/.npm-global"

EXPOSE 8080

USER "${USER}"

# Pre-create the target dir for global install.
RUN mkdir -p "${NPM_CONFIG_PREFIX}/lib"

# Create book directory
WORKDIR /book

# Install global packages
RUN npm --global config set user "${USER}"
RUN npm -g install gatsby-cli

# Install development packages
# COPY package*.json ./

# RUN npm ci

# # Bundle app source
# COPY . .

# Build static files
# RUN npm run build

# serve on port 8080
# CMD ["serve", "-l", "tcp://0.0.0.0:8080", "public"]
CMD /bin/bash -c "npm install && gatsby serve --verbose --prefix-paths -p 8080 --host 0.0.0.0"
