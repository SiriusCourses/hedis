{ pkgs ? import ./. {} }:
let self = rec {
  inherit pkgs;
  docker-cheops-email = pkgs.docker-cheops-email;
  };
in self
