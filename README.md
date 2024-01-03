# MRJP - Latte compiler for LLVM
Compiler Construction - 7. semester @ MIMUW

## Language description
<https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2023/Latte/>

## Compilation
The program is built after executing `make`. Firstly some of the source files are generated from the grammar of the language by BNFC. Then GHC compiles the code, creating `latc_llvm` executable in the root directory of the project. To remove all of the generated files, execute `make clean`.

## How to run
```
./latc_llvm foo/bar/baz.lat
```
It will create file `foo/bar/baz.ll` with generated LLVM code and bitcode file `foo/bar/baz.bc`. To interpret it, execute `lli foo/bar/baz.bc`.

## Tools and libraries
- GHC
- BNFC (`cabal install BNFC`)
- mtl (`cabal install mtl`)
- dlist (`cabal install dlist`)

## Project structure
Source files of the project are in `src/Compiler` directory. BNFC files generated from `src/Latte.cf` will be placed in `src/Latte`. GHC output files will be placed in `build` directory.

## Implemented extensions
- arrays
- structs
- front-end for objects
