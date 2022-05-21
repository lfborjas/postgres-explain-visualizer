let

    nixpkgs = import ./nix/packages.nix {};
    lib = import ./nix/release.nix;

in

    nixpkgs.haskellPackages.shellFor {
        packages = p: builtins.attrValues lib;
        buildInputs = with nixpkgs; [
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.implicit-hie
            nodejs
            hlint
            haskellPackages.fourmolu
        ];
    }
