let
  pkgs = import ./packages.nix {};
in
  { postgres-explain-visualizer = pkgs.haskellPackages.postgres-explain-visualizer; }
