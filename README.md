# CPP lang [![](https://github.com/monadplus/CPP-lang/actions/workflows/test.yml/badge.svg)](https://github.com/monadplus/CPP-lang/actions)

## Project

**CPP lang** is a static type-checked general purpose programming language and my very first programming language.

The language is similar to C but missing lots of important features such as:

- Arrays and pointers
- Structs and unions
- Syscalls and bindings with lib.c
- Native execution on x86 architectures

The grammar of the language can be found at [CPP.cf](CPP.cf).

### Structure

-   The frontend (lexer/parser) is defined at [Lexer.x](src/CPP/Lexer.x) and [Parser.y](src/CPP/Parser.y).
-   The type checker is defined at [TypeChecker.hs](src/CPP/TypeChecker.hs).
-   The interpreter is defined at [Interpreter.hs](src/CPP/Interpreter.hs).
-   The backend for the JVM is defined at [JVM/CodeGenerator.hs](src/CPP/JVM/CodeGen.hs).

## Compile

The compiler is written in *Haskell*. You can build the compiler using both [Cabal](https://cabal.readthedocs.io/en/3.4/):

```bash
$ cabal build
```

or [Stack](https://docs.haskellstack.org/en/stable/README/):

```bash
$ stack build
```

## Run the interpreter

Have a look at the files from [examples](examples/). 
All of them can be executed on the interpreter.

```bash
$ cabal run CPP -- <name>.cc --interpreter
```

## Run on the JVM

First, generate the `*.jar`:

```bash
$ cabal run CPP -- <name>.cc --jvm
```

Then, run the `*.jar`:

```bash
$ java -jar <name>.jar
```

## TODO

- [ ] Arrays
- [ ] Pointers
- [ ] Add `char` as primitive type
- [ ] =Value= should depend on =TExp=
- [ ] The JVM backend is not properly tested
