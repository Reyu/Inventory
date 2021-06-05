{
  description = "Home Inventory Management";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlays = [ ];
        pkgs =
          import nixpkgs { inherit system overlays; config.allowBroken = false; };
        project = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            inherit returnShellEnv;
            name = "Inventory";
            root = ./.;
            withHoogle = true;
            overrides =
              let
                haskell-beam = pkgs.fetchFromGitHub {
                  owner = "haskell-beam";
                  repo = "beam";
                  rev = "f461925cf94a096238b8c37f30318c46d1e9f9cf";
                  sha256 = "WFegq92+IuNK3uFguEfkVNIkBD0s0VMhWrYUeWiN04U=";
                  fetchSubmodules = true;
                };
                beam = self: subpkg: self.callCabal2nix "beam-${subpkg}" (haskell-beam + "/beam-${subpkg}") { };
              in
              self: super: with pkgs.haskell.lib; {
                # Use callCabal2nix to override Haskell dependencies here
                # cf. https://tek.brick.do/K3VXJd8mEKO7

                # We want to follow HEAD on the Beam repo, it has better features.
                beam-core =
                  pkgs.haskell.lib.dontCheck (
                    self.callCabal2nix "beam-core" (haskell-beam + "/beam-core") { });
                beam-postgres =
                  pkgs.haskell.lib.dontCheck (
                    self.callCabal2nix "beam-postgres" (haskell-beam + "/beam-postgres") { });
                beam-migrate =
                  pkgs.haskell.lib.dontCheck (
                    self.callCabal2nix "beam-migrate" (haskell-beam + "/beam-migrate") { });
                beam-migrate-cli =
                  pkgs.haskell.lib.dontCheck (
                    self.callCabal2nix "beam-migrate-cli" (haskell-beam + "/beam-migrate-cli") { });
              };
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
              [
                # Specify build/dev dependencies here.
                cabal-fmt
                cabal-install
                ghcid
                haskell-language-server
                pkgs.nixpkgs-fmt
              ]);
          };
      in
      {
        # Used by `nix build` & `nix run` (prod exe)
        defaultPackage = project false;

        # Used by `nix develop` (dev shell)
        devShell = project true;
      });
}
