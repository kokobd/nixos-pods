{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    {
      nixosConfigurations.primary =
        nixpkgs.lib.nixosSystem {
          system = flake-utils.lib.system.x86_64-linux;
          modules = [ ./configuration.nix ];
        };
    };
}
