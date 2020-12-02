{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
   nativeBuildInputs = with pkgs.haskellPackages; [
     brittany
     cabal-install
     ghcid
   ];
}
