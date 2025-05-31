# minivi

A simple terminal-based text editor (A vim clone) written in Haskell.

## Requirements

- unix-like os (i.e. `macOS`, `linux` or WSL on Windows)
- `ghc 9.4.8` (not tested with other `ghc` vesions)
- `cabal 3.12` (any 3.0+ version should work)

## Demo
<p align="center">
  <img src="./demo.gif" alt="minivi demo" />
</p>

## Building and Running

To build the project:

```sh
$ cabal build
```

To edit a file (e.g. `test.txt`):

``` sh
$ cabal exec minivi -- test.txt
```

You can also use `cabal repl` to start ghci.

To run tests:

```sh
$ cabal test
```

## Goals

- Content rendering `renderContent`
  - replace tabs with spaces
  - generate content buffer to be rendered
- Cursor update `updateCursor`
- Editing `handleInsert`
  - insertion
  - deletion
  - return
- Commands `handleCommand`
  - write file
  - quit
  - force quit
- Simple features
  - start program without initial file and save to file using `:w file`
  - press `w` (or `b`) to go to the start of next/last word, and use `0` (or `$`) to jump to the first/last character of the line.