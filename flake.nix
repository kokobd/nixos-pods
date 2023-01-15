{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/22.11";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    nix-bundle = {
      url = "github:matthewbauer/nix-bundle";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = inputs@{ self, nixpkgs, nix-bundle, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        make = pkgs:
          let
            dhall = import ./nix/dhall.nix { inherit pkgs; };

            cabalPackage =
            pkgs.haskell.lib.compose.overrideCabal (old: old // {
              preBuild = ''
                export DHALL_PACKAGE_PATH=${dhall}/source.dhall
              '';
            }) 
              ((pkgs.haskell.packages.ghc92.override {
                packageSetConfig = final: prev: {
                  withHoogle = true;
                };
                overrides = import ./nix/haskell-overrides.nix { inherit pkgs; };
              }).callCabal2nix "nixos-pods" ./pkg
                { });

            pickExecutable = name:
              pkgs.runCommand name { } ''
                cp ${cabalPackage}/bin/${name} $out
              '';
          in
          {
            env = cabalPackage.env;
            bin = name:
              nix-bundle.defaultBundler {
                program = pickExecutable name;
                inherit system;
              };
            bin-raw = pickExecutable;
          };

        deploy-base =
          pkgs.writeScriptBin "deploy-base" ''
            ${(make pkgs).bin-raw "deploy"} base --dhall-file ${./base.dhall} $@
          '';

        # dhall = pkgs.buildDhallPackage 
      in
      {
        apps = {
          deploy-base = {
            type = "app";
            program = "${deploy-base}/bin/deploy-base";
          };
        };

        packages =
          {
            data-compressor-lambda = (make pkgs).bin "data-compressor-lambda";
            data-compressor-job = (make pkgs).bin "data-compressor-job";
            dhall = import ./nix/dhall.nix { inherit pkgs; };
          };

        devShells = import ./nix/shells.nix make pkgs;
      });
}
