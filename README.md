# hs-db

## Overview

`hs-db` is a Haskell-based application designed to interact with a PostgreSQL database. It provides functionalities to parse environment variables, manage database connections, and execute SQL queries. The application is structured with a library and an executable, and it includes a test suite.

## Project Structure

- **app/**: Contains the main application entry point.
- **src/**: Contains the source code for the library.
- **test/**: Contains the test suite.
- **sql/**: Contains SQL scripts used by the application.
- **.vscode/**: Contains VSCode tasks for building and testing.
- **compose.yaml**: Docker Compose configuration for setting up the database environment.
- **nodemon.json**: Configuration for nodemon to watch file changes.
- **package.json**: Node.js dependencies for development.

## TODO:

- [ ] Implement a way to fetch logs from another service
- [x] Mount the database's logs locally in docker compose
- [ ] Add comments to the parser
- [ ] Convert type aliases to newtypes


## Prerequisites

- Haskell and Cabal installed on your system.
- Docker and Docker Compose for setting up the database.
- Node.js and npm for managing JavaScript dependencies.

## Setup

1. **Clone the repository:**

   ```bash
   git clone <repository-url>
   cd hs-db
   ```

2. **Install Node.js dependencies:**

   ```bash
   npm install
   ```

3. **Set up the database using Docker Compose:**

   ```bash
   docker-compose -f compose.yaml up -d
   ```

4. **Create a `.env` file:**

   The application reads environment variables from a `.env` file. Ensure you have the following variables set:

   ```
   DB_USER=user1
   DB_PASSWORD=user1
   DB_HOST=0.0.0.0
   DB_PORT=5433
   DB_NAME=user1
   ```

## Building and Running

### Using Cabal

1. **Build the project:**

   ```bash
   cabal build
   ```

2. **Run the application:**

   ```bash
   cabal run
   ```
