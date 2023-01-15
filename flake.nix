{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/22.11";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        dhall = import ./nix/dhall.nix { inherit pkgs; };
        make = pkgs:
          let
            cabalPackage =
              pkgs.haskell.lib.compose.overrideCabal
                (old: old // {
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
          rec {
            env = cabalPackage.env;
            bin = pickExecutable;
            dockerImage = name: pkgs.dockerTools.buildImage {
              inherit name;
              config = {
                Cmd = [ "${bin name}" ];
              };
            };
          };

        tool = (make pkgs).bin "tool";
      in
      {
        apps =
          let toolApp = {
            type = "app";
            program = "${tool}";
          };
          in
          {
            default = toolApp;
            tool = toolApp;
          };

        packages =
          {
            inherit tool;
            data-compressor-lambda = (make pkgs).dockerImage "data-compressor-lambda";
            data-compressor-job = (make pkgs).bin "data-compressor-job";
            inherit dhall;
          };

        devShells = import ./nix/shells.nix make pkgs;
      });
}
