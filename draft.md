# DRAFT

issues:
- turnt.toml unclosed string python panics

setup.sh:
pipx install turnt  # 1.11.0

??:
turnt simulate typing enter (test multiline string input to repl)
racket readline

interpreter testing infra (instin)
    you see interpreter project, you remember this infra
    mal, aam, tapl
    ghci, iex, racket
    all optional: repl, bench, colored std out/err
    without plugins:
        input program directly in ast
        tests organized as in source unittest
        debug print to visualize small steps
        utilize testcases for extra value (docs, pbt/fuzz/higher level techniques)
        per host language idioms for fs, logging instrumentation
        ?? can you formalize interpreters, whatever that means?
        ?? for compiled world there's ffi. how can interpreters be not monolingual.
