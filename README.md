# PLC to LLVM compiler

Compiles Plutus Core (PLC) programs to LLVM. Both, typed and untyped, PLC programs are accepted in both, serialised or plain-text, formats.

## Prerequisites

On macOS, install system dependencies with `brew install llvm@12 libsodium gmp`.

On Ubuntu, install `libsodium` with `sudo apt-get install -y libsodium-dev libgmp-dev` and LLVM with:

```
wget https://apt.llvm.org/llvm.sh
chmod +x llvm.sh
sudo ./llvm.sh 12
```

## How to build

The easiest way to build this program is by just running `stack build`.

If you are on macOS, make sure that LLVM 12 executables are in your `$PATH`. For example, you might wish to run `PATH=/usr/local/opt/llvm@12/bin:$PATH stack build` or add the extra path to the Stack configuration (which will also better support Haskell Language Server):

```yaml
extra-path:
- /usr/local/opt/llvm@12/bin
```

(Optionally) you may wish to copy the compiler binary into a convenient location with e.g. `stack install --local-bin-path=.` (replace `.` with the path to the directory you want).

## Usage

Assuming you have installed the `plc-llvm` binary somewhere where it can be found, the most basic usage of the compiler is with (where `INPUT` is a placeholder for a filename):

```
$ plc-llvm INPUT
```

If not, you can also substitute `plc-llvm` for `stack run --`. The input named by `INPUT` is assumed to be an Untyped Plutus Core program in plain-text format. By default, `plc-llvm` will generate an executable in the same directory as the source file with the same name. The compiler will also produce LLVM Bitcode (.bc) and an object file (.o) in the process.

A more sophisticated example which shows some of the options supported by the compiler is:

```
$ plc-llvm INPUT --emit-llvm --rts './compiler/rts/rts.c' --verbose --trace
```

- The `--emit-llvm` flag tells the compiler to output plain-text LLVM IR (.ll) instead of the (binary) LLVM Bitcode (.bc)
- The `--rts './compiler/rts/rts.c'` flag tells `plc-llvm` where to find the runtime system.
- The `--verbose` flag tells `plc-llvm` to print diagnostic messages while it is compiling the program.
- The `--trace` flag tells `plc-llvm` to emit tracing code in the generated LLVM IR.

If the compiler is run with no arguments or the `--help` flag is specified, information about all available options is shown.

## Usage as a library

Rather than just generating a binary and running that, you may want to just compile the PLC code and link it together with e.g. a C program. This is supported by the `--library` flag, which will omit the default entry point, skip linking, and add additional exports which make it easier to programmatically interact with the PLC program. For example, consider a PLC program such as the following which requires one additional argument to compute something useful:

```
(program 1.0.0
    (lam x [(builtin addInteger) (con integer 4) x])
)
```

Compiling this to an executable is not very useful since the program is already a normal form. However, if we compile it with the `--library` flag we will instead receive an object file and a C header. For example, assuming that the above PLC program is saved as `add0.plc`, we can compile it with `plc-llvm add0.plc --library` to receive `add0.o` and `add0.h`. We can then write a C program which imports the header and defines an entry point for the program:

```c
// include the header that was generated by plc-llvm
#include "add0.h"

int main() {
    // allocate a new integer closure, this will be our extra argument
    closure *arg = plc_new_integer("104");

    // run the PLC program and acquire a pointer to the resulting closure
    closure *ptr = plc_entry();

    // apply the resulting closure to the extra argument, which also returns a closure
    closure *res = plc_apply(ptr, arg);

    // invoke the closure's pretty-printing function
    plc_print_closure(res);
    putchar('\n');

    return 0;
}
```

We can now compile this C program and link it together with `add0.o`. To simplify this process, `plc-llvm` provides an extra option which can automate this entire process. Assuming that our C program is saved as `program.c`, we can run `plc-llvm add0.plc --library --entry-point=program.c` to compile both files and link everything together.

## Regression tests

You can run all the tests with `stack test`. The source files for all of the regression tests may be found in `./compiler/test-data/`.
