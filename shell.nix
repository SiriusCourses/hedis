{ pkgs ? import ./nix {} }:
pkgs.haskellPackages.shellFor {
   packages = _ : [pkgs.haskellPackages.cheops-email];
   nativeBuildInputs = with pkgs.haskellPackages; [
     cabal-install
     ghcid
   ];
}
