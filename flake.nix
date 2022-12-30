{
  description = "A very basic flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        hocketDrv = pkgs.haskell.lib.justStaticExecutables
          (pkgs.haskellPackages.callCabal2nix "hocket" ./. { });

        devEnv = pkgs.haskellPackages.developPackage {
          returnShellEnv = true;
          root = ./.;
          modifier = with pkgs.haskell.lib;
            drv:
            dontHaddock (disableOptimization (disableLibraryProfiling drv));
        };
      in rec {
        apps.hocket = pkgs.haskell.lib.justStaticExecutables hocketDrv;
        defaultApp = apps.hocket;

        packages.hocket = hocketDrv;
        defaultPackage = packages.hocket;

        devShell = devEnv;
      });
}
