{ pkgs ? import <nixpkgs> { } }:
let
  als = pkgs.haskellPackages.callPackage ./als.nix { };

  nixImage = pkgs.dockerTools.pullImage {
    imageName = "nixorg/nix";
    imageDigest = "sha256:119e3299b14173dd37e7ea6aca64d41d171f8eac2323ad02d06d25efd0862f98";
    finalImageName = "nix";
    finalImageTag = "latest";
    sha256 = "1l4yrg7yzk25cp6ww1gxf730narv3jijmskb1vjmvx8nnpsmv8yv";
  };
in
pkgs.dockerTools.buildImage {
  name = "asmundberge/als";

  fromImage = nixImage;
  fromImageName = "nixorg/nix";
  fromImageTag = "latest";

  created = "now";
  contents = [ als ];
  config = {
    Cmd = [
      "${als}/bin/als" 
    ];
  };
}
