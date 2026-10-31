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

### EPUB

The `--epub` flag makes the compiler output XHTML files, which `build-epub.sh` packages into an EPUB file together with the stylesheet from `book/epub` and the processed illustrations. If [EPUBCheck](https://github.com/w3c/epubcheck) is installed, the script also validates the result.

```bash
mkdir -p epub-out
cd compiler
cabal v2-run haskellBook.cabal -- --epub "../book/" "../epub-out/"
cd ..
./build-epub.sh epub-out public/img programiranje_u_haskelu-nikola_ubavic.epub
```

## Atex

Atex is a markup language that supports basic text formatting. It is inspired by the TeX, but unlike the TeX, in Atex commands start with the `@`. Hence the language name (`at + tex`).

Atex supports following commands:
 + Sectioning: `@chapter`, `@section`, `@subsection`
 + Block environments: `@codeBlock`, `@terminal`, `@example`, `@problem`, `@figure`, `@list`
 + Paragraph `@p` and list item `@li` environments
 + Markup: `@em` (emphasis), `@code` (inline Haskel code), `@pre` (general monospaced text) `@m` (inline math), `@eq` (block math), `@def` (term definition), `@url` (link), `@note` (sidenote), `@ref` (reference - not yet implemented).
 + Todo mark `@todo` - not rendered

Atex extension for VS Code adds support for syntax highlighting and set of snippets. The extension can be installed by copying `vscode-atex` directory to VSC settings directory:

```bash
cp vscode-atex/. ~/.vscode-oss/extensions/atex/ -r
```
