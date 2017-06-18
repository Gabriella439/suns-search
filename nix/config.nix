{ packageOverrides = pkgs: rec {
    filterSource = pkgs.callPackage ./filter-source.nix { };

    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: {
        blas = pkgs.haskell.lib.doJailbreak haskellPackagesOld.blas;
        hmatrix = haskellPackagesNew.callPackage ./hmatrix.nix { };

        suns-search =
          pkgs.haskell.lib.overrideCabal
            (haskellPackagesNew.callPackage ../default.nix { })
            (oldDerivation:
              if oldDerivation ? src
              then {
                  src = filterSource oldDerivation.src;
                }
              else {}
            );
      };
    };
  };
}
