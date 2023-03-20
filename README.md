# calc-lexer-rkt
Simple infix to prefix lexer/rearranger in Racket.

The core infix calculation lexer is heavily inspired by Matt Might's [Lexical analysis in Racket](https://matt.might.net/articles/lexers-in-racket/) with some modifications for tracking nested parenthesised expressions.

The procedure `infix->prefix` just parses the lexer output from left to right and doesn't really handle operator precedence at all.

## How to run:
Currently there is no friendly command line interface to the program, so you'll have to load the file into the Racket repl and run `(lex-test-all)` or `(calc-test-all)` to inspect lexed and rearranged output respectively. Alternatively you could run your own tests with `(print-lex-test <input>)` and `(print-calc-test <input>)`.

## Example:
```
> (calc-test-all)
1 + (4 - 3) / (7 ^ 3) => 
  (+ 1 (/ (- 4 3) (^ 7 3)))

(1 + (4 - 3) / (7 ^ 3)) => 
  (+ 1 (/ (- 4 3) (^ 7 3)))

π * (r ^ 2) => 
  (* π (^ r 2))

(e ^ (π * i)) = 0 => 
  (= (^ e (* π i)) 0)

'(#<void> #<void> #<void> #<void>)
```

## Known issues:
- While the lexer can handle the lack of some whitespace, an expression like `1-3` is converted into `'((INT 1) (INT -3))` which is (currently) unhandled.
- The expression used to denote exponentiation '`^`' is really a placeholder and isn't defined in Racket, so if you want to `eval` the result of any expression containing this you'll have to define '`^`' appropriately.
