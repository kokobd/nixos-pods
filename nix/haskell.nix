{ system
, nixpkgs
, dhall
}:
let
  pkgs = nixpkgs.legacyPackages.${system};
  make = pkgs:
    let
      cabalPackage =
        pkgs.haskell.lib.compose.dontHaddock
          (
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
                { })
          );
    in
    {
      pkg = cabalPackage;
      env = cabalPackage.env;
      dockerImage = name: pkgs.dockerTools.streamLayeredImage {
        inherit name;
        config = {
          Cmd = [ "${cabalPackage}/bin/${name}" ];
        };
      };
    };
  project = make pkgs;
  tool = pkgs.writeShellScript "tool" ''
    SERVICE_IMAGES_DIR="${services}/images"
    ${project.pkg}/bin/tool "$@"
  '';
  services =
    pkgs.runCommand "nixos-pods-serivces" { } (
      "mkdir -p $out/images\n" +
      builtins.concatStringsSep "\n" (
        builtins.map
          (name: "cp ${project.dockerImage name} $out/images/${name};")
          (pkgs.dhallToNix (builtins.readFile ../dhall/services.dhall))
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

  devShells.default = project.env.overrideAttrs (oldAttrs: {
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
