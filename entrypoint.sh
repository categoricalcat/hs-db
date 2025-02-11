#!/bin/sh

# --- Configuration (Environment Variables) ---
RUNNER_TOKEN="${RUNNER_TOKEN:?Error: RUNNER_TOKEN environment variable is not set}"
RUNNER_OS="${RUNNER_OS:?Error: RUNNER_OS environment variable is not set}"
RUNNER_ARCH="${RUNNER_ARCH:?Error: RUNNER_ARCH environment variable is not set}"
RUNNER_VERSION="${RUNNER_VERSION:?Error: RUNNER_VERSION environment variable is not set}"
RUNNER_NAME="hs-db-${RUNNER_OS}-${RUNNER_ARCH}-${RUNNER_VERSION}"
APPLICATION_URL="https://github.com/categoricalcat/hs-db" # Default URL

remove_runner() {
  echo "Removing runner $RUNNER_TOKEN"
  ./config.sh remove --token "$RUNNER_TOKEN"
}

configure() {
  echo "Running config.sh..."
  ./config.sh \
    --url "$APPLICATION_URL" \
    --token "$RUNNER_TOKEN" \
    --replace \
    --name "$RUNNER_NAME" \
    --unattended
}

# --- Cleanup Function ---
cleanup() {
  echo "Container stopping... Removing runner #$RUNNER_TOKEN"

  # Assuming config.sh is available in the PATH in your Docker image
  remove_runner

  if [ $? -ne 0 ]; then
    echo "Warning [./config.sh]: Remove command may have failed at cleanup" >&2
    # Do NOT exit here in cleanup, allow container to stop gracefully
  fi

  echo "Cleanup completed."
}

trap cleanup SIGTERM SIGINT

# --- Main Script Logic ---
echo "Starting Docker entrypoint script..."

# --- Run Configuration Script ---
configure

if [ $? -ne 0 ]; then
  echo "Error [./config.sh]: Configure command failed, removing runner $RUNNER_TOKEN" >&2
  remove_runner
  exit 1
fi

# --- Run Application Script ---
echo "Running run.sh..."
./run.sh
if [ $? -ne 0 ]; then
  echo "Error [./run.sh]: Run command failed, removing runner $RUNNER_TOKEN" >&2
  remove_runner
  exit 1
fi

# --- Explicit Exit ---
echo "Entrypoint script has reached end of execution, removing runner $RUNNER_TOKEN"
remove_runner
exit 0
