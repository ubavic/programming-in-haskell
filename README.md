# Programming in Haskell

This is repositorium for the *[Programiranje u Haskelu](https://haskel.ubavic.rs)*. The book is written in custom format `.atex` and compiled to `.html` via custom software written in Haskell.

*Programing in Haskell* is written on Serbian, and it is intended as an introductory text to Haskell programming.

Monorepo contains following packages:

+ `book`: text source for the book
+ `compiler`: compiler for `.atex` format
+ `vscode-atex`: VS Code extension for `.atex` format
+ `public`: JS, CSS and image files. Compiled html is placed here

## Build

In `compiler` directory run

```bash
cabal v2-run haskellBook.cabal -- "../book/" "../public/"
```
