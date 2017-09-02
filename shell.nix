{ nixpkgs ? import <nixpkgs> { }
, compiler ? "default"
, interpreter ? "default"
}:
let
  inherit (nixpkgs) pkgs;
  inherit (nixpkgs) stdenv;

  pythonPackages = if interpreter == "default"
                      then pkgs.pythonPackages
                      else pkgs."${interpreter}Packages";

  dungeon-studio = import ./default.nix { inherit nixpkgs compiler; };

in
  stdenv.mkDerivation rec {
    name = "dungeon-studio-shell";
    buildInputs = [
      dungeon-studio
      pythonPackages.docker_compose
      pythonPackages.requests
    ];
  }
