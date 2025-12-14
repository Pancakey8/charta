# Charta

```
fn main () {
→ 3 ⇈       0 > ? "Go!" put
    -           ↓
    1
    ↑ put ⇈     ←
}
```

## Overview

Charta is a stack-based, 2D language. The language aims to achieve:

1. Clear & concise control flow. Visually, the code should explain what it does.
2. Feature completeness. The language shouldn't have to <sup>(but still
   might)</sup> provide any non-trivial operations internally. An operation such
   as reversing a stack should be user implementable.
3. Total stack dependency. Instead of providing additional registers, variables,
   or secondary means of storage; the language instead provides first-class
   support for stacks and functions. This way, higher-level code can be
   expressed in a more stack-oriented manner. 
   
Interested? See [docs/](docs/Get-Started.md)

## Compiling the Codebase

Make sure you have Haskell toolchain installed.
Easiest way to install is through [GHCUp](https://www.haskell.org/ghcup/).

Build with Cabal:

```sh
cabal build charta -O2
```

Executable is then found in:

```sh
cabal list-bin charta
```

## Contributing

Because the project is in an early stage / experimental, I don't intend on
taking pull requests currently. Issues are always welcome if you find them.
