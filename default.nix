{ nixpkgs ? import <nixpkgs> { }, compiler ? "ghc883" }:

let drv = nixpkgs.haskell.packages.${compiler}.callCabal2nix "hocket" ./. { };
in drv
