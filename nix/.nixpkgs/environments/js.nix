{ pkgs ? (import <nixpkgs> {}) }:

{
  jsEnv = pkgs.stdenv.mkDerivation {
    name = "js-env";
    buildInputs = with pkgs; [
      nodejs
      nodePackages.webpack
      nodePackages.yarn
    ];
  };
}
