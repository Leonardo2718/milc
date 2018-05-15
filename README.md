# Milc - the M Intermediate Language Compiler

(CPSC 411 project, University of Calgary, Winter 2018)

Milc is a small compiler for the [M language](http://pages.cpsc.ucalgary.ca/~robin/class/411/M+/Mspec.pdf)
written in Haskell. It generates code for the Cookie Virtual Machine.

## Feature overview

Some note-worthy features include:

- can read source from multiple files or standard input
- recognizes nested multi-line comments
- reports missing `*/` for each level of un-closed nested comment
- gives single-line comments higher precedence over multi-line comments
- reports line and column number of errors
- reports file containing the reported error (if not reading from standard input)
- shows source code line(s) where an error is found
- uses an expressive intermediate language called MIL
- can do a few simple optimizations
- generates code to `*.ckie` files
- can generate detailed log files of compilations (use `-l` option)
- supports various command line options (use `--help` for more details)

## Feature disclaimer

Although milc can parse and type-check both arrays and user defined data-types,
which are both part of the M++ language, it will not generate code for them and
throw an error if used.

Also, milc does not guarantee evaluation order for sub-expressions and function
parameters as the M language does not define these. Calling functions with
visible side effects in sub-expressions should, therefore, be avoided.

## Dependencies

Milc has the following dependencies:

- [Alex *(build time only)*](http://hackage.haskell.org/package/alex)
- [array](http://hackage.haskell.org/package/array)
- [mtl (version 2.0 or greater)](http://hackage.haskell.org/package/mtl)
- [filepath](http://hackage.haskell.org/package/filepath)
- [unordered-containers (version 0.2.7.0 or greater)](http://hackage.haskell.org/package/unordered-containers)

## Building

There are two ways of building Milc, both of which generate an executable called
`milc`. The (*highly*) recommended way is to use `cabal` as it will take care of
handling all package dependencies. Alternatively, a Makefile is also provided.

### Using `cabal`

This is the recommended way of building `milc`. You may *optionally* create a
cabal sandbox and install all dependencies inside it:

```
$ cabal sandbox init
$ cabal install --dependencies-only
```

To build `milc`, run:

```
$ cabal configure
$ cabal build
```

This will produce a directory called `dist/` that contains all build outputs,
including the `milc` executable (`dist/build/milc/milc`).

### Using `make`

If you choose to use `make` instead of `cabal`, **make sure you have all the
required dependencies installed on your system**.

By default, the Makefile will put all build outputs in a directory called
`make_build/`. This can be changed by setting the `OUTPUT_DIR` environment
variable to the desired path. To build, simply run `make`.

## Running

The format for invoking the compiler is `milc [OPTIONS] [SOURCES]`.
`cabal run --` may also be used to invoke `milc` (make sure all `milc` specific
command line options go after the `--`). If no source files are provided, `milc`
will read source code from standard input up-to EOF (`Ctrl+D` on most systems).
For a complete list of available command line options, use the `--help` option:

```
$ cabal run -- --help
```

To compile some example files, run:

```
$ mkdir out
$ cabal run -- test/good/* -d out -l
```

This will compile all the test programs in `test/good/` and put all output files
in the `out/` directory. It will also generate a log file called `milc.log`.

## Running a sample program

The following commands show how to run the Mandelbrot test on the CPSC servers:

```
$ mkdir out
$ cabal run -- test/good/mandelbrot.mpp -d out -l
$ alias AM='/usr/bin/sml @SMLload=/home/411/AM/am+.x86-linux'
$ AM out/mandelbrot.am
```

Enjoy the view!

## Tests

The `test/` directory contains some sample tests for the compiler. Tests are
organized in several sub-directories:

- `test/unimplemented_features`: contains tests for partially supported features
- `test/good`: contains "well-formed" test files that should compile successfully
- `test/bad`: contains test files that should cause compilation failures
- `test/py`: contains Python 3 scripts that produce the same output as some of the tests
    - these are mostly useful for figuring out expected outputs

## License

The source code for milc and all accompanying scripts and documentation are
distributed under the terms of [the MIT License](https://opensource.org/licenses/MIT).
