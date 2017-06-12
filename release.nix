# You can build this repository using Nix by running:
#
#     $ nix-build -A suns-search release.nix
#
# You can also open up this repository inside of a Nix shell by running:
#
#     $ nix-shell -A suns-search.env release.nix
#
# ... and then Nix will supply the correct Haskell development environment for
# you
let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          suns-search = haskellPackagesNew.callPackage ./default.nix { };
        };
      };
    };
  };

  pkgs =
    import <nixpkgs> { inherit config; };

in
  { suns-search = pkgs.haskellPackages.suns-search;
  }
