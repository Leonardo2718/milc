# Milc - the Minisculus Intermediate Language Compiler

Milc is a small compiler for the Minisculus language written in Haskell. It
targets Robin's Stack Machine and generates code as a self-contained csh script.

## Feature overview

Some note-worthy features include:

- can read source from multiple files or standard input
- recognizes nested multi-line comments
- reports missing `*/` for each level of un-closed nested comment
- gives single-line comments higher precedence than multi-line comments
- reports line and column number of errors
- reports file containing the reported error (if not reading from standard input)
- shows source code line(s) where an error is found
- uses an expressive intermediate language called MIL
- can generate detailed logs files of compilations (use `-l` option)
- supports various command line options (use `--help` for more details)

## Dependencies

Milc has the following dependencies:

- Alex *(build time only)*
- array
- mtl (version 2.0 or greater)
- filepath

## Building

There are two ways of building Milc, both of which generate an executable called
`milc`. The (*highly*) recommended was is to use cabal as it will take care of
checking the correct dependencies are installed. Alternatively, a Makefile is
also provided.

### Using cabal

This is the recommended way of building `milc`. You may *optionally* create a
cabal sandbox and install all dependencies inside it:

```
$ cabal sandbox init
$ cabal install --dependencies-only
```

To build the package, run:

```
$ cabal configure
$ cabal build
```

This will produce a directory called `dist/` that contains all build outputs,
including the `milc` executable (`dist/build/milc/milc`).

### Using make

If you choose to use make instead of cabal, **make sure you have all required
dependencies installed on your system**.

By default, the Makefile will put all build output files in a directory called
`make_build`. This can be changed by setting the `OUTPUT_DIR` variable to the
desired path. To build, simply run `make`.

## Running

The format for invoking the compiler is `milc [OPTIONS] [SOURCES]`.
`cabal run --` may also be used to run the compiled (make sure all `milc`
command line options go after the `--`). If no source files are provided, `milc`
will read source code from standard input up-to EOF (`Ctrl+D` on most systems).
For a complete list of available command line options, use the `--help` option:

```
$ cabal run -- --help
```

To compile some example files, run:

```
$ mkdir out
$ cabal run -- test/good/* -d out
```

This will compile all the test programs in `test/good/` and put all output files
in the `out/` directory. If no output directory is specified, the output files
will be placed in the same directory as the source files.

## Tests

The `tests/` directory contains some sample tests for the compiler. Tests are
further divided into three sub-directories:

- `test/from_assignment`: contains test files provided as part of the assignment
- `test/good`: contains "well-formed" test files that should compile successfully
- `test/bad`: contains test files that should cause compilation failures

## License

The source code for milc and all accompanying scripts and documentation are
distributed under the terms of [the MIT License](https://opensource.org/licenses/MIT).
