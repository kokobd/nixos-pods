{ pkgs }: final: prev:
let
  mkSubDirPackage = name: subdir: src:
    {
      inherit name;
      value = prev.callCabal2nix name
        (
          pkgs.stdenv.mkDerivation {
            inherit name;
            inherit src;
            installPhase = ''
              mkdir -p $out
              cp -r ${subdir}/. $out
            '';
          })
        { };
    };
  amazonka-src = pkgs.fetchFromGitHub {
    owner = "brendanhay";
    repo = "amazonka";
    rev = "3a56aa905be5177c5132fbf0bd0e7aff00bd1274";
    sha256 = "sha256-jM6qyQZY1fcLn4CVgH4AHDrfN0/6yJarvgMR4EexQAc=";
  };
  amazonkaPackage = name: mkSubDirPackage name ("lib/" + name) amazonka-src;
  amazonkaServicePackage = name: mkSubDirPackage name ("lib/services/" + name) amazonka-src;
  wai-handler-hal-src = pkgs.fetchFromGitHub {
    owner = "bellroy";
    repo = "wai-handler-hal";
    rev = "e79c658880707084d4155080b7c6ad6bc9c2d8cd";
    sha256 = "sha256-zMpD+Q9ZJwNi7t2dWhIf7aDfG193M0QtX1ybFVUSygg=";
  };
in
builtins.listToAttrs
  [
    (amazonkaPackage "amazonka")
    (amazonkaPackage "amazonka-core")
    (amazonkaPackage "amazonka-test")
    (amazonkaServicePackage "amazonka-sso")
    (amazonkaServicePackage "amazonka-sts")
    (amazonkaServicePackage "amazonka-s3")
    (amazonkaServicePackage "amazonka-ec2")
    (amazonkaServicePackage "amazonka-dynamodb")
    (amazonkaServicePackage "amazonka-cloudformation")
    (amazonkaServicePackage "amazonka-ecr")
    (mkSubDirPackage "wai-handler-hal" "wai-handler-hal" wai-handler-hal-src)
  ] // {
  pretty-simple = prev.callHackage "pretty-simple" "4.1.2.0" { };
  dhall-json = prev.callHackage "dhall-json" "1.7.11" { };
  dhall = prev.callHackage "dhall" "1.41.2" { };
}
