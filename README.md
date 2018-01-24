# mcomp - The Minisculus Compiler

In its current state, the compiler only runs lexical analyzer.

## Building

The following are descriptions of different procedures for building the compiler,
all of which produce a binary called `mcomp`. Note that the Alex lexer generator
is a *required* build dependency.

### Using cabal

This is the recommended way of building `mcomp`.

```
cabal configure
cabal build
```

This will produce a directory called `dist/` that contains all the build outputs,
including the `mcomp` executable (`dist/build/mcomp/mcomp`).

### Using make

An alternative way of building `mcomp` is to use the provided Makefile. By default,
it will put all the build output files in a directory called `make_build`. This
can be changed by setting the `OUTPUT_DIR` variable to the desired path.

```
make
```

## Running

The format for invoking the compiler is `mcomp [OPTIONS] [SOURCES]`. If no source
files are provided, `mcomp` will read source code from standard input up-to EOF
(`Ctrl+D` on most systems). For a complete list of available command line options,
run `mcomp --help`.

### Using cabal

If using `cabal` to build `mcomp`, the easiest way to run the program is to use
`cabal run --`. Everything after the `--` will be passed through as an option to
the `mcomp` executable.

```
$ cabal run -- --help                           # will print the help message
$ cabal run -- test/from_assignment/test3.m_    # will compile the file test/from_assignment/test3.m_
```

### Running manually

Assuming make was used to build `mcomp`, an equivalent way of running the above
commands is:

```
$ ./make_build/mcomp --help
$ ./make_build/mcomp test/from_assignment/test3.m_
```
