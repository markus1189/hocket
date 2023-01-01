{
  description = "A very basic flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        hsPkgs = pkgs.haskellPackages;

        hocketDrv = pkgs.haskell.lib.justStaticExecutables
          (hsPkgs.callCabal2nix "hocket" ./. { });

        devEnv = hsPkgs.developPackage {
          returnShellEnv = true;
          root = ./.;
          modifier = with pkgs.haskell.lib;
            drv:
            dontHaddock (disableOptimization (disableLibraryProfiling drv));
        };
      in rec {
        apps.hocket = {
          type = "app";
          program = "${pkgs.haskell.lib.justStaticExecutables hocketDrv}/bin/hocket";
        };
        defaultApp = apps.hocket;

        packages.hocket = hocketDrv;
        defaultPackage = packages.hocket;

        devShell = pkgs.mkShell {
          inputsFrom = [ devEnv ];
          buildInputs = with hsPkgs; [ haskell-language-server ];
        };
      });
}
