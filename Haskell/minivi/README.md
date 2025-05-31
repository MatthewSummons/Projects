# minivi

A simple terminal-based text editor (A vim clone) written in Haskell.

## Requirements

- unix-like os (i.e. `macOS`, `linux` or WSL on Windows)
- `ghc 9.4.8` (not tested with other `ghc` vesions)
- `cabal 3.12` (any 3.0+ version should work)

## Demo
<p align="center">
  <img src="./demo.gif" alt="minivi demo" />
  <!-- ![til](./demo.gif) -->
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

- content rendering `renderContent`
  - replace tabs with spaces
  - generate content buffer to be rendered
- cursor update `updateCursor`
- editing `handleInsert`
  - insertion
  - deletion
  - return
- commands `handleCommand`
  - write file
  - quit
  - force quit

## Bonus

- simple features
  - start program without initial file and save to file using `:w file`
  - press `w` (or `b`) to go to the start of next/last word, and use `0` (or `$`) to jump to the first/last character of the line.
- moderate features
  - press `u` to undo and `U` to redo, and keep a history round 4 steps
  - press `/` to search, and `n` (or `N`) to jump to next/last match

## Submission

Submit a single zip file containing the project. You can use the following command (replace `XXX` with your UID):

```sh
$ zip -r A3_XXX.zip README.md minivi.cabal app src test c
```
