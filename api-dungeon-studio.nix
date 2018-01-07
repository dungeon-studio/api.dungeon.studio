let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackageOld: rec {

          api-dungeon-studio =
            haskellPackagesNew.callPackage ./default.nix { };

          hasbolt =
            haskellPackagesNew.callPackage ./hasbolt.nix { };

          network-arbitrary =
            haskellPackagesNew.callPackage ./network-arbitrary.nix { };

          network-uri-json =
            haskellPackagesNew.callPackage ./network-uri-json.nix { };

          siren-json =
            haskellPackagesNew.callPackage ./siren-json.nix { };

        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
in
  { api-dungeon-studio = pkgs.haskellPackages.api-dungeon-studio;
  }
