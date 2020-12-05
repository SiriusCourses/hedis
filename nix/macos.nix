{ pkgs ? import ./. {}}:
let self = rec {
      inherit pkgs;
      cheops-email = pkgs.haskellPackages.cheops-email;
};
in self
