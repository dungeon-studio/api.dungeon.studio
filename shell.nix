{ nixpkgs ? import <nixpkgs> { }
, compiler ? "default"
}:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
in
  (import ./default.nix { inherit nixpkgs compiler; }).env
