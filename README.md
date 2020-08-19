## hscalc

[![asciicast](https://asciinema.org/a/354394.svg)](https://asciinema.org/a/354394)

A simple command-line REPL calculator utility with various styles of implementation. 
Monadic parsing, functors, combinators, and lambda expressions galore!

Not ready for real use yet.

There are two implementations so far. 
One of them is a naive recursive parsing implementation (no monadic parsing). 
This is the utility you'll get if you compile this project (in `/src/Lib.hs`).
With the exception of parens, this utility should respect the order of operations and have the 4 basic arithmetic expressions.

```bash
stack build
stack exec hscalc-exe
```

The other one is in `/src/Parser.hs` and is a start at a proper monadic recursive parser.
The monadic parser still needs some work.

--- 

TODO
- [ ] proper parens parsing
- [ ] get monadic parser to respect order of operations
- [ ] allow for efficient factoring and checking if prime (may be useful for CTFs)

