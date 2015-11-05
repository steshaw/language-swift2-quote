Quasiquotation for Swift in Haskell
===================================

This library will eventually provide quasiquotation support for Swift in Template Haskell.

Resources
---------

- [Swift 2.1 Grammar Summary](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/zzSummaryOfTheGrammar.html)


TODO
----

- remaining `pattern` productions.
- Replace interim `identifier` parser with one that meets the spec.
- `getter-setter-keyword-block` (currently disguised as simply `getter-setter-block`).
- `raw-value-style-enum`
- expressions need to use `chainr` and `chainl`.
- missing/incomplete rendering functions.
- test cases
  - closures and closure signatures
  - availability conditions
  - protocol members
  - nested expressions
