#!/usr/bin/env bash
# Source this script before running the Cabal tests, or execute it with a command.
if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then set -euo pipefail; fi
als_broker_dir=$(mktemp -d)
export RABBITMQ_NODENAME="als_test@localhost"
export RABBITMQ_NODE_PORT=5679
export RABBITMQ_DIST_PORT=25679
export ERL_EPMD_PORT=43679
export RABBITMQ_SERVER_ADDITIONAL_ERL_ARGS="+S 2:2 +A 2 -setcookie als-disposable-test-cookie"
export RABBITMQ_CTL_ERL_ARGS="+S 2:2 +A 2 -setcookie als-disposable-test-cookie"
export RABBITMQ_MNESIA_BASE="$als_broker_dir/mnesia"
export RABBITMQ_LOG_BASE="$als_broker_dir/log"
export RABBITMQ_PID_FILE="$als_broker_dir/rabbit.pid"
export RABBITMQ_ENABLED_PLUGINS_FILE="$als_broker_dir/plugins"
export RABBITMQ_CONFIG_FILE="$als_broker_dir/rabbitmq"
printf 'listeners.tcp.default = 5679\nloopback_users.guest = true\n' > "$RABBITMQ_CONFIG_FILE.conf"
printf '[].\n' > "$RABBITMQ_ENABLED_PLUGINS_FILE"
cleanup_als_broker() {
  rabbitmqctl shutdown >/dev/null 2>&1 || true
  wait "$als_broker_pid" 2>/dev/null || true
  epmd -kill >/dev/null 2>&1 || true
}
trap cleanup_als_broker EXIT
rabbitmq-server > "$als_broker_dir/server.log" 2>&1 &
als_broker_pid=$!
als_broker_ready=false
for als_attempt in $(seq 1 60); do
  if rabbitmq-diagnostics -q ping >/dev/null 2>&1; then
    als_broker_ready=true
    break
  fi
  if ! kill -0 "$als_broker_pid" 2>/dev/null; then
    cat "$als_broker_dir/server.log"
    exit 1
  fi
  sleep 1
done
if [ "$als_broker_ready" != true ]; then
  cat "$als_broker_dir/server.log"
  exit 1
fi
export ALS_INTEGRATION_PORT=5679
if [[ "${BASH_SOURCE[0]}" == "$0" ]] && [ "$#" -gt 0 ]; then
  "$@"
fi
