Quasiquotation for Swift in Haskell
===================================

This library will eventually provide quasiquotation support for Swift in Template Haskell.


TODO
----

- `condition-clause` other than `expression` for `while-statement`.
- `getter-setter-keyword-block` (currently disguised as simply `getter-setter-block`).
- `raw-value-style-enum`
- remaining `pattern` productions.
- `attribute`
- `closure-expression`
- Replace interim `identifier` parser with one that meets the spec.
- missing/incomplete rendering functions.
- much testing and bug fixing
  - expressions need to use `chainr` and `chainl`.
