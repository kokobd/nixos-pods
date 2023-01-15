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
      in
      nixpkgs.lib.attrsets.recursiveUpdate
        (import ./nix/haskell.nix {
          inherit system;
          inherit nixpkgs;
          inherit dhall;
        })
        {
          packages.dhall = dhall;
        }
    );
}
