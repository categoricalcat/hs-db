#!/bin/sh

# --- Configuration (Environment Variables) ---
RUNNER_TOKEN="${RUNNER_TOKEN:?Error: RUNNER_TOKEN environment variable is not set}"
RUNNER_OS="${RUNNER_OS:?Error: RUNNER_OS environment variable is not set}"
RUNNER_ARCH="${RUNNER_ARCH:?Error: RUNNER_ARCH environment variable is not set}"
RUNNER_VERSION="${RUNNER_VERSION:?Error: RUNNER_VERSION environment variable is not set}"
RUNNER_NAME="hs-db-${RUNNER_OS}-${RUNNER_ARCH}-${RUNNER_VERSION}"
APPLICATION_URL="https://github.com/categoricalcat/hs-db" # Default URL

# --- Cleanup Function ---
cleanup() {
  echo "Container stopping... Removing runner #$RUNNER_TOKEN"
  # Assuming config.sh is available in the PATH in your Docker image
  ./config.sh remove --token "$RUNNER_TOKEN"
  if [ $? -ne 0 ]; then
    echo "Warning: config.sh remove command may have failed." >&2
    # Do NOT exit here in cleanup, allow container to stop gracefully
  fi
  echo "Cleanup completed."
}

trap cleanup SIGTERM SIGINT

# --- Main Script Logic ---
echo "Starting Docker entrypoint script..."

# --- Run Configuration Script ---
echo "Running config.sh..."
./config.sh \
  --url "$APPLICATION_URL" \
  --token "$RUNNER_TOKEN" \
  --replace \
  --name "$RUNNER_NAME" \
  --unattended

if [ $? -ne 0 ]; then
  echo "Error: config.sh command failed. Exiting." >&2
  exit 1
fi

# --- Run Application Script ---
echo "Running run.sh..."
./run.sh
if [ $? -ne 0 ]; then
  echo "Error: run.sh command failed. Exiting." >&2
  exit 1
fi

# --- Explicit Exit ---
echo "Entrypoint script execution finished."
exit 0
