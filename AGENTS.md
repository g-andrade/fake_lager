# Agent guidance for fake_lager

## What this is

`fake_lager` is a drop-in replacement for the [`lager`](https://github.com/erlang-lager/lager/) logging library. Instead of running lager itself, it forwards all log calls to Erlang/OTP's built-in `logger`. The goal is to ease migration away from lager on codebases where a full rewrite is impractical.

## Commands

```bash
# Compile
make compile          # or: rebar3 compile

# Run all tests
make test             # eunit + common_test + coverage

# Run only common_test
make ct               # rebar3 do ct, cover

# Run only eunit
make eunit            # rebar3 eunit

# Run a single CT suite
rebar3 ct --suite test/fake_lager_tests_pr_SUITE

# CT suites (for reference)
#   fake_lager_tests_transform_SUITE  — parse transform AST rewriting
#   fake_lager_tests_lager_api_SUITE  — lager public API surface
#   fake_lager_tests_lager_msg_SUITE  — lager_msg backward-compat stub
#   fake_lager_tests_pr_SUITE         — record pretty-printing

# Format code
make format           # rebar3 fmt

# Fast static analysis (format, xref, hank, elvis)
make check-fast

# Slow static analysis (dialyzer only)
make check-slow

# All checks
make check
```

## Architecture

The library works through a **compile-time parse transform** (`lager_transform`) that rewrites lager logging calls into `logger` calls. Downstream modules opt in by adding `{parse_transform, lager_transform}` to their compiler options (or via rebar.config `erl_opts`).

### Core transformation flow

1. `lager_transform:parse_transform/2` walks the AST of any module that uses it.
2. It intercepts calls to `lager:debug/info/warning/...` (and extra sinks) and rewrites them to `logger:debug/info/warning/...`.
3. Log metadata (`mfa`, `file`, `line`, `lager_sink`) is injected as a map literal at compile time.
4. Each rewritten call is wrapped in a compile-time level gate — `case logger:allow(Level, Module) of true -> logger:macro_log(#{}, Level, Format, Args, Metadata); false -> ok end` — exactly as logger's own `?LOG_*` macros do (and as lager's original inlined level check did). The format string, arguments and metadata live only in the `true` branch, so they are evaluated exclusively when the level is enabled; there is no fun-wrapping and no argument-cost heuristic. The enabled branch dispatches via `logger:macro_log/5` (not `logger:Level/3`) so that the level check is performed only once — `logger:Level/3` would re-run `logger_config:allow/2` internally via `do_log/3`, whereas `macro_log/5` goes straight to `log_allowed/4`.

### Record pretty-printing

`lager:pr(Value, Module)` pretty-prints records as tagged maps, mirroring lager's original `pr` function:

- During the parse transform, every `-record(...)` definition encountered is collected into a `fake_lager_pr:context()`.
- That context is embedded into the compiled module as a hidden module attribute (`___$fake_lager.pr_context`).
- At runtime, `lager:pr/2,3` retrieves this attribute via `lager_transform:get_pr_context/1` and delegates to `fake_lager_pr:pr/3`.
- `fake_lager_pr` recursively pretty-prints nested records, lists, tuples, and maps.

### Module responsibilities

| Module | Role |
|---|---|
| `lager_transform` | Parse transform: AST rewriting, metadata injection, record def collection |
| `lager` | Public API surface — mirrors lager's API; most functions either redirect to `logger` or raise `error(notsup)` for unsupported lager features |
| `fake_lager_pr` | Record pretty-printing logic and context management |
| `lager_msg` | Stub module for backward compatibility with code that calls `lager_msg` directly |
| `fake_lager_app` / `fake_lager_sup` | OTP application and supervisor (minimal; the library is mostly compile-time) |

### OTP version considerations

- Supported: OTP 24–28 (OTP 21+ declared in `rebar.config` but only 24+ is truly supported per README).
- `rebar.config.script` conditionally removes `rebar3_hank`, `erlfmt`, and `rebar3_lint` plugins on OTP ≤ 25 where they are incompatible.
- `NO_CT_SUITE_BEHAVIOUR` is defined on OTP 22.x and early 23.x (no `ct_suite` behaviour).

### What is intentionally not supported

Several lager APIs (`tracing`, `rotate_*`, `set_loglevel`, etc.) exist as stubs that raise `error(notsup)`. This is by design — they have no meaningful equivalent in the `logger` world.
