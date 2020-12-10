{ pkgs ? import <nixpkgs> { } }:
with pkgs;

haskell.lib.justStaticExecutables
(haskellPackages.callCabal2nix "hocket" ./. { })
