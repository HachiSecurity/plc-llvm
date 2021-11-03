# PLC to LLVM compiler

Compiles Plutus Core programs to LLVM.

## Prerequisites

On macOS, install system dependencies with `brew install llvm@12 libsodium`.

On Ubuntu, install `libsodium` with `sudo apt-get install -y libsodium-dev` and LLVM with:

```
wget https://apt.llvm.org/llvm.sh
chmod +x llvm.sh
sudo ./llvm.sh 12
```

## How to build

The easiest way to build this program is with `stack build`. If you are on macOS, you should specify `--stack-yaml=stack-lts-17.14.macos.yaml` as an additional parameter.

## Usage

The most basic usage of the compiler is with (where `INPUT` is a placeholder for a filename):

```
$ stack run -- INPUT
```

The input is assumed to be an Untyped Plutus Core program in plain-text format. By default, `plc-llvm` will generate an executable in the same directory as the source file with the same name. The compiler will also produce LLVM Bitcode (.bc) and an object file (.o) in the process.

A more sophisticated example which shows some of the options supported by the compiler is:

```
$ stack run -- INPUT --emit-llvm --rts './compiler/rts/rts.c' --verbose --trace
```

- The `--emit-llvm` flag tells the compiler to output plain-text LLVM IR (.ll) instead of the (binary) LLVM Bitcode (.bc)
- The `--rts './compiler/rts/rts.c'` flag tells `plc-llvm` where to find the runtime system.
- The `--verbose` flag tells `plc-llvm` to print diagnostic messages while it is compiling the program.
- The `--trace` flag tells `plc-llvm` to emit tracing code in the generated LLVM IR.

If the compiler is run with no arguments or the `--help` flag is specified, information about all available options is shown.

## Regression tests

You can run all the tests with `stack test`. If you are on macOS, you should specify `--stack-yaml=stack-lts-17.14.macos.yaml` as an additional parameter. The source files for all of the regression tests may be found in `./compiler/test-data/`.
