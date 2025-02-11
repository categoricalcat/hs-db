FROM haskell:9.10.1-bullseye AS setup
WORKDIR /actions-runner

ARG RUNNER_VERSION=2.322.0
ARG RUNNER_OS=linux
ARG RUNNER_ARCH=x64


RUN apt-get update -y && \
  apt-get upgrade -y && \
  apt-get install -y \
  curl \
  ca-certificates \
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
  libncurses-dev \
  libicu-dev \
  libkrb5-dev \
  ssh && \
  # Install Node.js
  curl -fsSL https://deb.nodesource.com/setup_20.x | bash - && \
  apt-get install -y nodejs && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

RUN curl -O -L https://github.com/actions/runner/releases/download/v${RUNNER_VERSION}/actions-runner-${RUNNER_OS}-${RUNNER_ARCH}-${RUNNER_VERSION}.tar.gz && \
  tar xzf actions-runner-${RUNNER_OS}-${RUNNER_ARCH}-${RUNNER_VERSION}.tar.gz && \
  rm actions-runner-${RUNNER_OS}-${RUNNER_ARCH}-${RUNNER_VERSION}.tar.gz && \
  ./bin/installdependencies.sh

RUN useradd -m runner && \
  chown -R runner:runner /actions-runner

# Install dependencies
FROM setup AS install
WORKDIR /app

COPY . .

RUN npm i -g npm pnpm

RUN pnpm install

RUN cabal update && cabal build

# Runner image
FROM setup AS runner
WORKDIR /actions-runner

USER root
COPY entrypoint.sh .
RUN chmod +x entrypoint.sh

USER runner
ENTRYPOINT ["./entrypoint.sh"]

# Development image
FROM install AS dev
WORKDIR /app


EXPOSE 8000
CMD ["pnpm",  "dev"]
