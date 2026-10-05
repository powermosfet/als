# Aquisitive List Service

ALS is a Haskell RabbitMQ worker that creates Microsoft To Do tasks. It consumes
an **existing** queue, processes one delivery at a time with prefetch one, and
acknowledges a delivery only after Graph returns a successfully decoded task.
Every valid delivery creates a new task, even when titles repeat.

```sh
nix build
nix run
nix develop
nix flake check
```

The flake pins `nixos-unstable` in `flake.lock` and exposes a default package, app,
development shell (GHC, Cabal, HLS, HLint, RabbitMQ and Python), and checks on
`x86_64-linux` and `aarch64-linux`. Checks run the unit/HTTP tests and a disposable
RabbitMQ integration harness on the host platform. No Microsoft credentials are
needed for tests. ALS does not include a NixOS service module or container image.

## Configuration

Set these variables before running `als` or `nix run`:

| Variable | Default / requirement |
| --- | --- |
| `RABBITMQ_HOST` | `localhost` |
| `RABBITMQ_PORT` | `5672`; integer from 1 to 65535 |
| `RABBITMQ_VHOST` | `/` |
| `RABBITMQ_USERNAME` | `guest` |
| `RABBITMQ_PASSWORD` | `guest` |
| `RABBITMQ_QUEUE` | `shopping-list-items` |
| `RETRY_DELAY_SECONDS` | `5`; positive integer |
| `HTTP_TIMEOUT_SECONDS` | `30`; positive integer |
| `LIST_ID` | Required for the worker: Microsoft To Do list ID |
| `CLIENT_ID` | Required: Microsoft application/client ID |
| `ACCESS_TOKEN`, `REFRESH_TOKEN` | Required unless loaded from `TOKEN_FILE` |
| `TOKEN_FILE` | Optional path to persistent JSON tokens |
| `REDIRECT_URL` | Required only for `--auth`; must match your app registration |

Blank settings and invalid numbers fail startup with a nonzero exit status.
Timeouts must fit in the platform's integer number of microseconds. Retry delays
must fit in a platform integer. Validation and token loading happen before the
broker connection is opened. Tokens and RabbitMQ passwords are never logged by
the worker. Logs contain lifecycle events and failure categories, not message
bodies or HTTP error bodies.

## Authentication and token storage

Register a Microsoft public-client application with delegated `Tasks.ReadWrite`
and `offline_access` permissions and a redirect URL. Run:

```sh
export CLIENT_ID='your-application-id'
export REDIRECT_URL='your-registered-redirect-url'
export TOKEN_FILE="$PWD/tokens.json"
nix run -- --auth
```

Open the printed authorization URL in a browser, consent, and paste **only the
`code` query parameter** from the redirect URL (URL-decode it first). ALS uses no
HTTP listener. With `TOKEN_FILE`, it saves the resulting access and refresh
tokens; without it, it prints them once for manual setup. `LIST_ID` is not needed
for this setup command. Find your list ID with Graph's
[`GET /me/todo/lists`](https://learn.microsoft.com/en-us/graph/api/todo-list-lists?view=graph-rest-1.0).

The token file has this shape:

```json
{"access_token":"...","refresh_token":"..."}
```

An existing file takes precedence over environment tokens. A missing file
bootstraps from `ACCESS_TOKEN` and `REFRESH_TOKEN`; a corrupt or unreadable file
fails startup. Its parent directory must already exist and be writable. On a
401 response, ALS refreshes once and retries the task request once. Both returned
tokens are retained in memory. When configured, the token file is replaced
atomically with permissions `0600` before another task request. A persistence
failure stops the worker and retains the delivery. Without a token file, refreshed
tokens survive only for the current process lifetime.

Creation decodes Graph's direct
[task response](https://learn.microsoft.com/en-us/graph/api/todotasklist-post-tasks?view=graph-rest-1.0).
Refresh replaces both tokens following Microsoft's
[token refresh guidance](https://learn.microsoft.com/en-us/entra/identity-platform/refresh-tokens).

## Queue and messages

Provision the queue, virtual host, permissions, and any routing before starting
ALS. ALS does not declare queues, exchanges or bindings. Local RabbitMQ's `guest`
user is suitable for the default localhost configuration.

Publish UTF-8 JSON to the configured queue:

```json
{"description":"Milk 🥛"}
{"description":"  Apples  ","source":"shopping app","quantity":2}
```

The description becomes the title exactly, including surrounding whitespace.
Additional attributes are ignored. Malformed JSON, a non-object body, a missing
or nonstring description, and empty or whitespace-only descriptions are rejected
without requeue. Graph item-validation errors (400/422) are rejected likewise.
RabbitMQ may dead-letter rejected messages if an administrator has configured a
policy for the queue; ALS does not configure that policy.

Network failures, 408, 429 and Microsoft 5xx responses retry while retaining the
unacknowledged delivery. The delay is at least `RETRY_DELAY_SECONDS`, and respects
`Retry-After` seconds or HTTP dates. Authentication, permission, missing-list,
invalid-success-response and token-persistence failures leave the delivery
unacknowledged and exit nonzero. Correct the configuration/credentials before
restarting. Broker disconnections and consumer cancellation trigger reconnection.
SIGINT/SIGTERM cancel processing and close the connection; unfinished deliveries
remain unacknowledged and can be delivered again.

Delivery is **at least once**. A crash or lost response after Microsoft creates a
task can cause a duplicate on retry. No deduplication store is included.

## Tests

```sh
nix flake check
# Unit tests and local HTTP fixtures in the development shell:
nix develop --command cabal test --offline
# Full broker integration harness in the development shell:
nix develop --command bash test/integration.sh cabal test --offline
```

The full check starts a disposable RabbitMQ node in a temporary directory with
ports 5679 (AMQP), 25679 (distribution) and 43679 (EPMD). These ports must be free
for a manual run. Its queue and dead-letter configuration are created only by the
test harness. It verifies repeated titles, acknowledgement, rejection, prefetch
one, redelivery after interruption, and reconnection after restarting the broker
application. The Graph stub binds a dynamically assigned loopback port. The
broker is stopped on exit. The production executable always uses Microsoft's
HTTPS endpoints; local endpoints are injected only through the test API.
