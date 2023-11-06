# lisp-interpreter

A lisp interpreter that aims to be a working (superset of) [mal] whose implementation isn't
necessarily inspiring for translation from racket to non-language-oriented programming languages.

The implementation has 2 interfaces:
- A repl to type programs in.
    - `racket repl.rkt` starts the repl.
    - We manage testing in this way using [turnt]. `turnt mal_process/*/*.t` runs all the tests.
- An implementation of racket's #lang protocol.
    - Usage is `racket program.mal`.

[mal]: https://github.com/kanaka/mal
[turnt]: https://github.com/cucapra/turnt
