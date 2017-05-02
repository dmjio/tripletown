{ pkgs ? import <nixpkgs> {} }:
  pkgs.haskellPackages.callPackage ./tripletown.nix {}
