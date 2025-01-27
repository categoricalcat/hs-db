FROM haskell:bullseye

WORKDIR /app

COPY . .

RUN apt update

RUN apt install -y \
  postgresql-client \
  libpq-dev

RUN apt clean

RUN cabal update

RUN cabal configure

RUN cabal build -O2 -v

CMD ["cabal", "run", "hs-db", "-O2"]
