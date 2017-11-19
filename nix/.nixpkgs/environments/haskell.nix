{ pkgs ? (import <nixpkgs> {}) }:

{
  haskellEnv = pkgs.stdenv.mkDerivation {
    name = "haskell-env";
    buildInputs = with pkgs; [
      cabal-install
      cabal2nix
      ghc802Packages.ghc
      ghc802Packages.cpphs
      ghc802Packages.haddock
      ghc802Packages.hlint
      ghc802Packages.hscolour
      ghc802Packages.stylish-haskell
      ghc802Packages.weeder
      stack
    ];
  };
}
