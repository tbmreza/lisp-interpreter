# lisp-interpreter

A lisp interpreter that aims to be a working (superset of) [mal] whose implementation isn't
necessarily inspiring for translation from racket to non-language-oriented programming languages.
Full compliance to upstream mal spec is a non-goal, but we argue for some degree of it with our test
suite.

In addition to mal's grammar (which its author describe as clojure-inspired), this interpreter
implements as first-class syntax the following utilities:
1. `until`
    - haskell's "until p f yields the result of applying f until p holds."
<!-- 2. head:tail split -->
2. ...

This repo brings value to the world by being a testable code artifact. Also see [repl.rkt]
and of course the merged racket implementation of mal for more trusted examples.

## Highlight

**Direct style** (as contrasted to continuation-passing style) **tree-walk** interpreter

## Usage

The implementation has 2 interfaces:
- A repl to type programs in.
    - `racket repl.rkt` starts the repl.
    - We manage testing in this way using [turnt]. `turnt mal_process/*/*.t` runs all the tests.
- An implementation of racket's #lang protocol.
    - Usage is `racket program.mal`.

[mal]: https://github.com/kanaka/mal
[turnt]: https://github.com/cucapra/turnt
[repl.rkt]: https://github.com/racket/racket/blob/master/racket/collects/racket/repl.rkt
