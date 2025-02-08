# FROM hs-db:latest
FROM haskell:bullseye AS install

ARG RUNNER_VERSION=2.322.0
ARG RUNNER_OS=linux
ARG RUNNER_ARCH=x64
ARG RUNNER_TOKEN

ENV RUNNER_VERSION=${RUNNER_VERSION}
ENV RUNNER_OS=${RUNNER_OS}
ENV RUNNER_ARCH=${RUNNER_ARCH}
ENV RUNNER_TOKEN=${RUNNER_TOKEN}

WORKDIR /

RUN apt update -y && apt upgrade -y

RUN curl -fsSL https://deb.nodesource.com/setup_22.x | bash -

RUN apt install -y \
  curl \
  build-essential \
  libssl-dev \
  libffi-dev \
  python3 \
  python3-venv \
  python3-dev \
  python3-pip \
  jq \
  sudo \
  git \
  libgmp-dev \
  libpq-dev \
  postgresql-client \
  nodejs \
  libncurses-dev \
  libicu-dev \
  libkrb5-dev

RUN apt-get -yqq install ssh

RUN mkdir actions-runner && cd actions-runner \
  && curl -O -L https://github.com/actions/runner/releases/download/v${RUNNER_VERSION}/actions-runner-${RUNNER_OS}-${RUNNER_ARCH}-${RUNNER_VERSION}.tar.gz \
  && tar xzf ./actions-runner-${RUNNER_OS}-${RUNNER_ARCH}-${RUNNER_VERSION}.tar.gz

WORKDIR /actions-runner

RUN chmod 755 -R .

RUN ./bin/installdependencies.sh

RUN RUNNER_ALLOW_RUNASROOT=1 ./config.sh \
  --url https://github.com/categoricalcat/hs-db \
  --token $RUNNER_TOKEN \
  --replace \
  --name hs-db-${RUNNER_OS}-${RUNNER_ARCH}-${RUNNER_VERSION} \
  --unattended

RUN chmod 755 -R .

# going to the app directory to install node
WORKDIR /app

COPY . .

RUN npm i -g npm

RUN npm i

# RUNNER IMAGE
FROM install AS runner
WORKDIR /actions-runner

ENTRYPOINT ["./run.sh"]

# BUILD IMAGE
FROM install AS build
WORKDIR /app

RUN cabal update

RUN cabal configure

RUN cabal build

# DEV IMAGE
FROM build AS dev
WORKDIR /app

CMD ["npm", "run", "dev"]

