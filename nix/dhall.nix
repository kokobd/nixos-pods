{ pkgs }:

with pkgs.dhallPackages;
buildDhallDirectoryPackage {
  name = "nixos-pods-dhall";
  dependencies = [ dhall-cloudformation Prelude ];
  src = ../dhall;
  source = true;
}
