{ system
, nixpkgs
, dhall
}:
let
  pkgs = nixpkgs.legacyPackages.${system};
  serviceExeNames = [ "store-pod-lambda" "store-pod-job" ];
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
            overrides = import ./haskell-overrides.nix { inherit pkgs; };
          }).callCabal2nix "nixos-pods" ../pkg
            { });

      pickExecutable = name:
        (pkgs.runCommand name { } ''
          cp ${cabalPackage}/bin/${name} $out
        '');
    in
    rec {
      env = cabalPackage.env;
      bin = pickExecutable;
      dockerImage = name: pkgs.dockerTools.buildLayeredImage {
        inherit name;
        config = {
          Cmd = [ "${bin name}" ];
        };
      };
    };
  tool = (make pkgs).bin "tool";
  services =
    pkgs.runCommand "nixos-pods-serivces" { } (
      "mkdir -p $out/images\n" +
      builtins.concatStringsSep "\n" (
        builtins.map
          (name: "cp ${(make pkgs).dockerImage name} $out/images/${name};")
          serviceExeNames
      )
    );
in
{
  apps.tool = {
    type = "app";
    program = "${tool}";
  };

  packages =
    {
      inherit tool;
      inherit services;
    };

  devShells.default = (make pkgs).env.overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [
      dhall
    ] ++ (
      with pkgs; [
        haskell.compiler.ghc92
        cabal-install
        nixpkgs-fmt
        dhall-json
        dhall-lsp-server
        awscli2
        rnix-lsp
        skopeo
      ]
    ) ++ (
      with pkgs.haskell.packages.ghc92; [
        cabal-fmt
        hlint
        haskell-language-server
      ]
    );
  });
}
