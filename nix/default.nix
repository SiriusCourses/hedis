with { fetch = import ./fetch.nix; };
{ nixpkgs ? fetch "nixpkgs" }:
import nixpkgs {
  config = { allowUnfree = true; enableParallelBuilding=true;  allowBroken = true; };
  # config = { allowUnfree = true; };
  overlays = import ./overlays.nix;
}
