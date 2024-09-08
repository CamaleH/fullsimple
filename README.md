# Fullsimple

Book: [Benjamin C. Pierce. 2002. Types and Programming Languages (1st. ed.). The MIT Press.](https://www.cis.upenn.edu/~bcpierce/tapl/)

PART II: Simple Types, Chapter 8 ~ 11

## Introduction
_fullsimple_ will implement parsing, type-checking and evaluating on simple typed
lambda expression. 

Now it supports following types:
1. Bool
2. Nat
3. Custom base types(Section 11.1)
4. Unit(Section 11.2)

## Techniques
1. Lexing with [Alex](https://hackage.haskell.org/package/alex)
2. parsing with [parsec](https://hackage.haskell.org/package/parsec).
3. The target form of parsing is Nameless Representation(Chapter 6)
