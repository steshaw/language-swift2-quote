# Quasiquotation for Swift in Haskell

This library will eventually provide quasiquotation support for Swift in
Template Haskell.

## Warning

Unfortunately the Swift 2.1 grammar is quite left recursive -- not just on the
small scale but on a large scale. This was not something I had noticed before
digging right into this project. It looks as though the changes for Swift 3
might make some things easier in this regard (though I haven't checked the
grammar closely). Much of this parser may be reusable for Swift 3. I would
recommend using a [grammar checking
tool](http://smlweb.cpsc.ucalgary.ca/start.html) prior to encoding the Swift 3
grammar in parser combinators.

## Resources

- [Swift 5.1 Grammar Summary](https://docs.swift.org/swift-book/ReferenceManual/zzSummaryOfTheGrammar.html)

## TODO

- [ ] Upgrade to 5.1 🤣.
- [ ] Fix "large scale" left-recursion.
- [ ] remaining `pattern` productions.
- [ ] Replace interim `identifier` parser with one that meets the spec.
- [ ] `getter-setter-keyword-block` (currently disguised as simply `getter-setter-block`).
- [ ] expressions need to use `chainr` and `chainl`.
- [ ] missing/incomplete rendering functions.
- [ ] additional test cases:
  - [ ] closures and closure signatures
  - [ ] availability conditions
  - [ ] protocol members
  - [ ] nested expressions
  - [ ] enums
