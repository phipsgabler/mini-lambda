[![No Maintenance Intended](http://unmaintained.tech/badge.svg)](http://unmaintained.tech/)

# Mini-Lambda #

The initial purpose of this project was just to try setting up a real-world Haskell project with stack, cleanly written and tested.

It ended up in an interesting task of understanding and implementing a normalizer for the untyped lambda calculus.

## Usage ##

There is a command-line interface to the normalizer, which can most easily be accessed via `stack exec mini-lambda`. Currently, this does only run the full reduction and prints the result. The syntax is defined as follows:

    EXPR = VAR
      | "(" LAMBDA VAR "." EXPR ")"
      | "(" EXPR EXPR ")"
    LAMBDA = "λ" | "\\"  (a single backspace)
    VAR = (['!'..'\''] | ['*'..'-'] | ['/'..'['] | [']'..'~'])+

This means that every lambda and every application have to be fully parenthesized -- this might get improved in the future. For more details, see [Parser.hs](https://github.com/phipsgabler/mini-lambda/blob/master/src/MiniLambda/Parser.hs) (the parser combinator used is self-written, just for fun, so there's poor error handling).
