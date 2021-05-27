{ pkgs ? import <nixpkgs> { } }:
  pkgs.haskellPackages.callPackage ./als.nix { }
