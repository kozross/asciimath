# `asciimath` 

## What is this thing?

An implementation of [AsciiMath](http://asciimath.org). We focus on having a
parser (based on `megaparsec`) and an AST; rendering is left to other packages.

## What are the goals of this project?

### Compatibility with AsciiMath

We aim to have exactly the same behaviour as AsciiMath's reference
implementation - anything else is a bug. Part of this effort also involves
documenting some of its corner cases, as its own documentation does not address
them. This is done, at least partly, with a suite of Hedgehog property tests.

### Minimal features

This package only provides a parser and AST: doing something with it is
considered someone else's responsibility. We provide some `optics` helpers, but
otherwise, no other functionality.

### Good error messages

Knowing why something went wrong is often half the battle: this applies even to
markup languages. The most-used-in-class tool for mathematical notation (LaTeX)
is _notoriously_ bad at this. We aim to give the clearest error messages we can.

## What works currently?

We have the ability to parse all 'leaf' constructions in AsciiMath as current.
This means anything that is a standalone construction, including letters,
numbers, function names, and any other pieces of syntax which don't require
anything else nearby.

Our parser works on `Text` only. Given that `text-2.0` has UTF-8 as its
representation, we pay no penalty for ASCII text: to ensure this, we do not
allow any version of `text` older than 2.0.

## What does this run on?

We plan support (and CI check) the following major versions of GHC:

* 8.10
* 9.0
* 9.2

We will check on the following platforms:

* Windows
* Linux
* MacOS

Currently, we cannot do this, as Hedgehog doesn't support `text-2.0`. As soon as
this is fixed, we will have CI as above.

## What can I do with this?

The project is licensed Apache 2.0 (SPDX code
[`Apache-2.0`](https://spdx.org/licenses/Apache-2.0.html)). For more details,
please see the `LICENSE.md` file.
