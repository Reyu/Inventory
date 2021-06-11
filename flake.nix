{
  description = "Home Inventory Management";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        haskell-beam = pkgs.fetchFromGitHub {
          owner = "haskell-beam";
          repo = "beam";
          rev = "f461925cf94a096238b8c37f30318c46d1e9f9cf";
          sha256 = "WFegq92+IuNK3uFguEfkVNIkBD0s0VMhWrYUeWiN04U=";
          fetchSubmodules = true;
        };
        dontCheck = pkgs.haskell.lib.dontCheck; # Allow package/deps marked as 'broken'
        overlay = final: prev: rec {
          haskellPackages = prev.haskellPackages // {
            beam-core = (pkgs.haskell.lib.overrideSrc prev.haskellPackages.beam-core {
              src = "${haskell-beam}/beam-core";
              version = "0.9.0.0";
            });
            # beam-postgres = prev.haskell.lib.doJailbreak (final.haskellPackages.callCabal2nix "beam-postgres" "${haskell-beam}/beam-postgres" { });
          };
        };
        pkgs = (import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        });
        haskellPackages = pkgs.haskellPackages;
      in rec {
        packages = flake-utils.lib.flattenTree {
          inventory = pkgs.stdenv.mkDerivation {
            pname = "Inventory";
            version = "0.0.0.0";
            src = ./.;

            libraryHaskellDepends = with haskellPackages; [
              beam-core
              aeson
              base
              directory
              dotenv
              postgresql-simple
              relude
            ];
            testHaskellDepends = with haskellPackages; [
              base
              hspec-core
              lens
              relude
            ];
            # testToolDepends = with haskellPackages; [ hspec-discover ];
            homepage = "https://github.com/Reyu/Inventory";
            # license = pkgs.lib.licenses.mit;
            builder = "${haskellPackages.cabal-install}/bin/cabal";
            args = [ "build" ];
          };
        };
        defaultPackage = packages.inventory;
      });
}
