{ pkgs }:

with pkgs.dhallPackages;
buildDhallDirectoryPackage {
  name = "nixos-pods";
  dependencies = [ dhall-cloudformation Prelude ];
  src = ../dhall;
  source = true;
}
