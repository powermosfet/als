{
  description = "ALS - Aquisitive List Service";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      eachSystem = nixpkgs.lib.genAttrs systems;
      package = system:
        let pkgs = import nixpkgs { inherit system; };
        in pkgs.haskellPackages.callCabal2nix "als" self { };
    in {
      nixosModules.default = { config, lib, pkgs, ... }@args:
        import ./nix/module.nix (args // { defaultPackage = package pkgs.stdenv.hostPlatform.system; });
      packages = eachSystem (system: { default = package system; });
      apps = eachSystem (system: {
        default = { type = "app"; meta.description = "RabbitMQ to Microsoft To Do worker"; program = "${package system}/bin/als"; };
      });
      checks = eachSystem (system:
        let pkgs = import nixpkgs { inherit system; };
        in {
          tests = package system;
          nixos-auth = import ./nix/test.nix { inherit pkgs; module = self.nixosModules.default; };
          integration = pkgs.haskell.lib.overrideCabal (package system) (old: {
            testToolDepends = (old.testToolDepends or []) ++ [ pkgs.rabbitmq-server pkgs.beamPackages.erlang ];
            preCheck = "source ${./test/integration.sh}";
          });
        });
      devShells = eachSystem (system:
        let pkgs = import nixpkgs { inherit system; };
        in { default = pkgs.haskellPackages.shellFor {
          packages = _: [ (package system) ];
          nativeBuildInputs = with pkgs; [ cabal-install haskell-language-server hlint
            rabbitmq-server beamPackages.erlang python3 ];
        }; });
    };
}
