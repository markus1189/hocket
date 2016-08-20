{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, async, attoparsec, base, brick
      , bytestring, case-insensitive, ConfigFile, containers
      , data-default, formatting, http-client, http-types, lens
      , lens-action, lens-aeson, microlens, mtl, network-uri, old-locale
      , process, sorted-list, stdenv, tasty, tasty-hspec, tasty-hunit
      , tasty-quickcheck, text, time, transformers, unordered-containers
      , vector, vty, wreq
      }:
      mkDerivation {
        pname = "hocket";
        version = "0.2.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson async attoparsec base brick bytestring ConfigFile containers
          data-default formatting http-client http-types lens lens-action
          lens-aeson microlens mtl network-uri old-locale process sorted-list
          text time transformers unordered-containers vector vty wreq
        ];
        executableHaskellDepends = [
          async base brick case-insensitive containers data-default
          formatting http-client lens lens-action lens-aeson microlens
          network-uri process text time transformers vty
        ];
        testHaskellDepends = [
          base containers lens tasty tasty-hspec tasty-hunit tasty-quickcheck
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
