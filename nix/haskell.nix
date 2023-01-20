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
    rec {
      env = cabalPackage.env;
      exe = name: pkgs.runCommand name { } ''
        cp ${cabalPackage}/bin/${name} $out
      '';
      dockerImage = name: pkgs.dockerTools.buildLayeredImage {
        inherit name;
        config = {
          Cmd = [ (exe name) ];
        };
      };
    };
  project = make pkgs;
  setToolEnvVars = ''
    export GZIP_PATH="${pkgs.gzip}/bin/gzip"
    export SKOPEO_PATH="${pkgs.skopeo}/bin/skopeo"
  '';
  tool = pkgs.writeShellScript "tool" ''
    export SERVICE_IMAGES_DIR="${services}/images"
    ${setToolEnvVars}
    ${project.exe "tool"} "$@"
  '';
  services =
    let toExeName = str:
      with pkgs.lib.strings;
      removePrefix "-"
        (
          concatMapStrings (ch: if toUpper ch == ch then "-" + toLower ch else ch)
            (
              stringToCharacters str
            )
        );
    in
    pkgs.runCommand "nixos-pods-serivces"
      { }
      (
        "mkdir -p $out/images\n" +
        builtins.concatStringsSep "\n" (
          builtins.map
            (
              name:
              let exeName = toExeName name; in
              "cp ${project.dockerImage exeName} $out/images/${exeName};"
            )
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
    shellHook = ''
      export SERVICE_IMAGES_DIR="./result/images"
      ${setToolEnvVars}
    '';
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
