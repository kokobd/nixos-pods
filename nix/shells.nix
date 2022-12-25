make: pkgs:
let nonHaskellTools = with pkgs;
[
  nixpkgs-fmt
  dhall
  dhall-json
  dhall-lsp-server
  awscli2
  rnix-lsp
];
in
{
  light = pkgs.mkShell {
    buildInputs = nonHaskellTools;
  };
  default = (make pkgs).env.overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs
      ++ (with pkgs; [
      cabal2nix
      cabal-install
      haskell.compiler.ghc92
    ])
      ++ nonHaskellTools
      ++ (with pkgs.haskell.packages.ghc92; [
      cabal-fmt
      hlint
      haskell-language-server
    ]);
  });
}
