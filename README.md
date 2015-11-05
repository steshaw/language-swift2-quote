Quasiquotation for Swift in Haskell
===================================

This library will eventually provide quasiquotation support for Swift in Template Haskell.

Resources
---------

- [Swift 2.1 Grammar Summary](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/zzSummaryOfTheGrammar.html)


TODO
----

- `getter-setter-keyword-block` (currently disguised as simply `getter-setter-block`).
- `raw-value-style-enum`
- remaining `pattern` productions.
- `closure-expression`
- Replace interim `identifier` parser with one that meets the spec.
- missing/incomplete rendering functions.
- much testing and bug fixing
  - expressions need to use `chainr` and `chainl`.
