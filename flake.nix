{
  description = "A very basic flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hocketPkg = pkgs.haskellPackages.callCabal2nix "hocket" ./. { };
        drv = pkgs.haskell.lib.justStaticExecutables hocketPkg;
      in rec {
        apps.hocket = pkgs.haskell.lib.justStaticExecutables drv;
        defaultApp = apps.hocket;

        packages.hocket = drv;
        defaultPackage = packages.hocket;

        devShell = pkgs.haskellPackages.developPackage {
          root = ./.;
          modifier = with pkgs.haskell.lib;
            drv:
            dontHaddock (disableOptimization (disableLibraryProfiling drv));
        };
      });
}
