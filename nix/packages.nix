{ sources ? import ./sources.nix, compiler ? "ghc8104", system ? builtins.currentSystem }:

let
  nixpkgs = import sources.nixpkgs { inherit config; overlay = [overlay]; system=system;};
  gitignoreSource = (import sources."gitignore.nix" {}).gitignoreSource;
  extra-deps = import ./extra-deps.nix {inherit system;};

  overlay = _: pkgs:
    {
      niv = (import sources.niv {}).niv;
    };
  config = { allowUnfree = true; allowBroken = true;
      packageOverrides = pkgs: rec {
        haskellPackages = pkgs.haskell.packages."${compiler}".override {
          overrides = self: super: (
            (extra-deps super) // {
              # automatically create a derivation based on the cabal file present in ../.
              postgres-explain-visualizer = (super.callCabal2nix "postgres-explain-visualizer" (gitignoreSource ../.) {});
            }
          );
        };
      };
  };
in nixpkgs
