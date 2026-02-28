# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

gdisco-el is an Emacs Lisp library that dynamically binds to Google APIs using the Google Discovery API. Rather than hand-coding wrappers for each endpoint, it fetches API specifications at runtime from `https://www.googleapis.com/discovery/v1/apis/{api}/{version}/rest` and constructs requests from the spec's metadata (parameters, paths, HTTP methods, base URLs).

Currently GET-only. Being redesigned toward a "Design D" architecture (see `BINDINGS-DESIGN.org`) with a uniform `gdisco-invoke` function inspired by Cognitect's Clojure aws-api.

## Dependencies

- `json` (built-in)
- `oauth2` (ELPA package)
- `s` (string manipulation; used for `s-replace` and `s-format` in URL template interpolation)

## Testing

No test infrastructure exists yet. The design document (`BINDINGS-DESIGN.org` § Testing Strategy) specifies:

- **Framework:** ERT (built into Emacs)
- **Test location:** `test/gdisco-test.el`
- **Fixtures:** Frozen Discovery JSON in `test/fixtures/`
- **Mocking:** `cl-letf` to rebind `gdisco--request-sync` (no external mocking library)

Planned commands (not yet implemented):
```sh
# Unit + integration tests
emacs -Q --batch -L . -l test/gdisco-test.el -f ert-run-tests-batch-and-exit

# Live tests (real Google credentials required)
GDISCO_LIVE_TESTS=1 emacs -Q --batch -L . -l test/gdisco-test.el -f ert-run-tests-batch-and-exit
```

## Architecture

### Current code (`gdisco.el`)

A single file with ~160 lines. The call flow for a GET request:

1. `gdisco-get-api` — fetches Discovery JSON for a service (e.g. "gmail" "v1"), parses it into an alist
2. `gdisco-get-path-to-method-node` — converts a dotted method name like `"users.messages.get"` into a spec traversal path: `(resources users resources messages methods get)`
3. `gdisco-traverse-path-to-api-node` — walks the parsed Discovery alist using that path to find the method node
4. `gdisco-get-parametrized-method-path` — interpolates path parameters (e.g. `{userId}`) into the URL template using `s-format`
5. `gdisco-get-query-string` — builds the query string from non-path parameters
6. `gdisco-do-get` — assembles the full URL, calls `oauth2-url-retrieve-synchronously`, parses JSON response

Parameters are passed as plists (`:userId "me"`). The Discovery spec uses alists (from `json-read`). The conversion between these two representations happens in `gdisco-get-method-arguments-for-path-or-query`, which checks each parameter's `location` field ("path" vs "query") against the spec.

### Planned architecture (Design D in `BINDINGS-DESIGN.org`)

Five public functions:
- `gdisco-auth` — OAuth wrapper
- `gdisco-client` — creates a `cl-defstruct` client (fetches + caches Discovery spec)
- `gdisco-invoke` — single function for all operations; HTTP verb auto-detected from spec; flat keyword params; sync by default, async via `:then`
- `gdisco-ops` — list available operations
- `gdisco-doc` — describe an operation's parameters

Key design decisions documented in `BINDINGS-DESIGN.org` § Key Design Decisions.

## Key Files

- `gdisco.el` — all current library code
- `example.el` — usage examples showing OAuth setup and API calls
- `BINDINGS-DESIGN.org` — full API design document with 4 candidate designs, comparison matrix, testing strategy, and phased implementation plan
- `REVIEW.org` — code review and assessment of current approach
