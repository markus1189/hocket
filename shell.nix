with import <nixpkgs> { };

haskellPackages.developPackage {
  root = ./.;
  modifier = with haskell.lib;
    drv:
    dontHaddock (disableOptimization (disableLibraryProfiling drv));
}
