{ nixpkgs ? import <nixpkgs> { }
, compiler ? "default"
, interpreter ? "default"
}:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  python = if interpreter == "default"
              then pkgs.python
              else pkgs.${interpreter};

in
  rec {
    dungeon-studio = import ./default.nix { inherit nixpkgs compiler; };
    
    myPython = python.withPackages (ps: [ps.docker_compose ps.requests]);

    env = myPython.env // dungeon-studio.env;
  }.env
