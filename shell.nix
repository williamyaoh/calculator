{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [
    haskell.compiler.ghc883
    pkg-config
    zlib
  ];
}
