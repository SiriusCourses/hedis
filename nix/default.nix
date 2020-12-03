with { fetch = import ./fetch.nix; };
{ nixpkgs ? fetch "nixpkgs" }:
import nixpkgs {
  config = { allowUnfree = true; enableParallelBuilding=true; };
  # config = { allowUnfree = true; };
  overlays = import ./overlays.nix;
}