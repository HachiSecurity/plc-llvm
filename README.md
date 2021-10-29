# PLC to LLVM compiler

Compiles Plutus Core programs to LLVM.

## Prerequisites

On macOS, install llvm with `brew install llvm@12 libsodium`.

## How to build

The easiest way to build this program is with `stack build`. You can also run all the tests with `stack test`. If you are on macOS, you should specify `--stack-yaml=stack-lts-17.14.macos.yaml` as an additional parameter in either case.

## Usage

To compile an Untyped Plutus Core program, run the following:

```
$ stack run -- code.plc --rts './compiler/rts/rts.c' --verbose --trace
```

- The `--rts './compiler/rts/rts.c'` flag tells `plc-llvm` where to find the runtime system.
- The `--verbose` flag tells `plc-llvm` to print diagnostic messages while it is compiling the program.
- The `--trace` flag tells `plc-llvm` to emit tracing code in the generated LLVM IR.

By default, `plc-llvm` will generate an executable in the same directory as the source file with the same name. In the example above, we would end up with `./code` which we can then run.
